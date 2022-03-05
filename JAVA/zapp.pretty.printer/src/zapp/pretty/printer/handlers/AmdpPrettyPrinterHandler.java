package zapp.pretty.printer.handlers;

import java.net.URI;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IProject;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.MultiPageEditorPart;
import org.eclipse.ui.texteditor.ITextEditor;


import com.sap.adt.tools.abapsource.ui.sources.editors.IAbapSourcePage;
import com.sap.adt.tools.core.project.IAbapProject;
import com.sap.adt.tools.core.ui.editors.IAdtFormEditor;
import com.sap.adt.communication.message.IResponse;
import com.sap.adt.communication.resources.AdtRestResourceFactory;
import com.sap.adt.communication.resources.IRestResource;
import com.sap.adt.communication.resources.IRestResourceFactory;
import com.sap.adt.communication.resources.ResourceNotFoundException;
import com.sap.adt.destinations.ui.logon.AdtLogonServiceUIFactory;
import com.sap.adt.project.IAdtCoreProject;
import com.sap.adt.project.ui.util.ProjectUtil;

@SuppressWarnings("restriction")
public class AmdpPrettyPrinterHandler extends AbstractHandler {

	private static final String PrettyPrinterURI = "/sap/bc/adt/zapp/zapp_pretty_printer";

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		// Get available projects in the workspace
		IProject project = getActiveAdtProject();

		// Trigger logon dialog if necessary
		ensureLoggedOn(project);

		IAbapProject abapProject = getAbapProject(project);

		IAbapSourcePage textEditor = getTextEditor(HandlerUtil.getActiveEditor(event));

		if (textEditor == null) {
			return null;
		}

		IDocument sourceCodeDoc = getDocument(textEditor);

		ITextSelection selection = (ITextSelection) textEditor.getSelectionProvider().getSelection();
		int beforeStartLine = -1;
		if (selection.getOffset() != -1) {
			beforeStartLine = selection.getStartLine();
		}

		if (sourceCodeDoc == null || abapProject == null) {
			displayError("Source Document not found");
			return null;
		}

		String sourceCodeOriginal = sourceCodeDoc.get();

		String sourceCodePretty = prettyPrint(abapProject, sourceCodeOriginal);

		try {
			if (sourceCodePretty != null) {
				
				int lineOffset;
				StyledText textControl = textEditor.getViewer().getTextWidget();
				int topIndex = textControl.getTopIndex();
				
				sourceCodeDoc.set(sourceCodePretty);
				
				if (sourceCodeDoc.getNumberOfLines() >= beforeStartLine) {
					lineOffset = sourceCodeDoc.getLineOffset(beforeStartLine);
				} else {

					lineOffset = sourceCodeDoc.getLineOffset(sourceCodeDoc.getNumberOfLines() - 1);
				}

				textEditor.getSelectionProvider().setSelection(new TextSelection(lineOffset, 0));
				textControl.setTopIndex(topIndex);
			}
		} catch (BadLocationException e) {
			displayError(e.getMessage());
		}
		return null;

	}

	private String prettyPrint(IAbapProject abapProject, String sourceCode) {

		// Create resource factory
		IRestResourceFactory restResourceFactory = AdtRestResourceFactory.createRestResourceFactory();

		// Create REST resource for given destination and URI
		String destination = abapProject.getDestinationId();
		URI prettyPrinterUri = URI.create(PrettyPrinterURI);
		IRestResource prettyPrinterResource = restResourceFactory.createResourceWithStatelessSession(prettyPrinterUri,
				destination);
		try {
			// Trigger Post request on resource data
			IResponse response = prettyPrinterResource.post(null, IResponse.class, sourceCode);

			return String.valueOf(response.getBody());

		} catch (ResourceNotFoundException e) {
			displayError("Error Calling Pretty Printer");
		} catch (RuntimeException e) {
			// Display any kind of other error
			displayError(e.getMessage());
		}
		return null;
	}

	private void displayError(String messageText) {
		String dialogTitle = "Pretty Printer Exception";
		openDialogWindow(messageText, dialogTitle);

	}

	protected void openDialogWindow(String dialogText, String dialogTitle) {
		String[] DIALOG_BUTTON_LABELS = new String[] { IDialogConstants.OK_LABEL };
		MessageDialog dialog = new MessageDialog(getShell(), dialogTitle, null, dialogText, MessageDialog.INFORMATION,
				DIALOG_BUTTON_LABELS, 0);
		dialog.open();
	}

	protected Shell getShell() {
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		return shell;
	}

	private static IProject getActiveAdtProject() {
		try {
			IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			IWorkbenchWindow window = page.getWorkbenchWindow();
			ISelection adtSelection = window.getSelectionService().getSelection();
			IProject project = ProjectUtil.getActiveAdtCoreProject(adtSelection, null, null,
					IAdtCoreProject.ABAP_PROJECT_NATURE);
			return project;
		} catch (Exception e) {
			return null;
		}
	}

	private static IAbapProject getAbapProject(IProject project) {

		return project.getAdapter(IAbapProject.class);

	}

	private static void ensureLoggedOn(IProject project) {
		try {
			IAbapProject abapProject = getAbapProject(project);
			AdtLogonServiceUIFactory.createLogonServiceUI().ensureLoggedOn(abapProject.getDestinationData(),
					PlatformUI.getWorkbench().getProgressService());
		} catch (Exception e) {
			System.out.print(e.getMessage());
		}
	}

	private IAbapSourcePage getTextEditor(IEditorPart editor) {
		IAbapSourcePage textEditor = null;
		if (editor instanceof MultiPageEditorPart) {
			MultiPageEditorPart multiPageEditor = (MultiPageEditorPart) editor;
			IEditorPart activePage = (IEditorPart) multiPageEditor.getSelectedPage();
			if (activePage instanceof IAbapSourcePage) {
				textEditor = (IAbapSourcePage) activePage;
			} else if (multiPageEditor instanceof IAdtFormEditor) {
				IEditorPart ed = ((IAdtFormEditor) multiPageEditor).getActiveEditor();
				if (ed instanceof IAbapSourcePage) {
					textEditor = (IAbapSourcePage) ed;
				}
			}
		} else if (editor instanceof IAbapSourcePage) {
			textEditor = (IAbapSourcePage) editor;
		}

		return textEditor;
	}

	private IDocument getDocument(ITextEditor editor) {
		return editor != null ? editor.getDocumentProvider().getDocument(editor.getEditorInput()) : null;
	}

}
