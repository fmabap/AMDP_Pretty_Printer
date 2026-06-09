package com.github.fmabap.amdpprettyprinter.eclipse.handlers;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.MultiPageEditorPart;
import org.eclipse.ui.texteditor.ITextEditor;

import com.sap.adt.tools.abapsource.ui.sources.editors.IAbapSourcePage;
import com.sap.adt.tools.core.ui.editors.IAdtFormEditor;

import com.github.fmabap.amdpprettyprinter.eclipse.Activator;
import com.github.fmabap.amdpprettyprinter.eclipse.preferences.PreferenceConstants;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.PrettyPrinter;
import com.github.fmabap.amdpprettyprinter.prettyprinter.Settings;

@SuppressWarnings("restriction")
public class AmdpPrettyPrinterHandler extends AbstractHandler {

	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {

		IAbapSourcePage textEditor = getTextEditor(HandlerUtil.getActiveEditor(event));
		if (textEditor == null) {
			return null;
		}

		IDocument sourceCodeDoc = getDocument(textEditor);
		if (sourceCodeDoc == null) {
			displayError("Source Document not found");
			return null;
		}

		ITextSelection selection = (ITextSelection) textEditor.getSelectionProvider().getSelection();
		int beforeStartLine = -1;
		if (selection.getOffset() != -1) {
			beforeStartLine = selection.getStartLine();
		}

		String sourceCodeOriginal = sourceCodeDoc.get();
		String sourceCodePretty = prettyPrintLocal(sourceCodeOriginal);

		try {
			if (sourceCodePretty != null) {
				StyledText textControl = textEditor.getViewer().getTextWidget();
				int topIndex = textControl.getTopIndex();

				sourceCodeDoc.set(sourceCodePretty);

				int lineOffset;
				if (beforeStartLine >= 0 && sourceCodeDoc.getNumberOfLines() > beforeStartLine) {
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

	/**
	 * Formats {@code sourceCode} using the local Java pretty printer core.
	 * The line-break-after-comma rule is read from the plugin preference store.
	 *
	 * @return formatted source, or {@code null} if formatting failed (error already displayed)
	 */
	private String prettyPrintLocal(String sourceCode) {
		String rule = Activator.getDefault().getPreferenceStore()
				.getString(PreferenceConstants.LB_AFTER_COMMA_RULE);
		if (rule == null || rule.isEmpty()) {
			rule = ISettings.LB_RULE_ALWAYS_LINE_BREAK;
		}
		boolean trace = Activator.getDefault().getPreferenceStore()
				.getBoolean(PreferenceConstants.TRACE);

		Settings settings = new Settings(rule, trace);

		// Split into lines without line delimiters; preserve trailing empty line if present
		List<String> lines = new ArrayList<>(Arrays.asList(sourceCode.split("\\r?\\n", -1)));

		PrintStream originalOut = System.out;
		try {
			if (trace) {
				System.setOut(getConsoleStream());
			}
			List<String> resultLines = new PrettyPrinter().prettyPrint(lines, settings);
			return String.join("\n", resultLines);
		} catch (AppException e) {
			displayError("Pretty Printer Error:\n\n" + e.getMessage());
			return null;
		} finally {
			if (trace) {
				System.setOut(originalOut);
			}
		}
	}

	private PrintStream getConsoleStream() {
		IConsoleManager consoleManager = ConsolePlugin.getDefault().getConsoleManager();
		String consoleName = "AMDP Pretty Printer";
		for (IConsole console : consoleManager.getConsoles()) {
			if (consoleName.equals(console.getName()) && console instanceof MessageConsole) {
				MessageConsole existing = (MessageConsole) console;
				return new PrintStream(existing.newMessageStream());
			}
		}
		MessageConsole console = new MessageConsole(consoleName, null);
		consoleManager.addConsoles(new IConsole[] { console });
		consoleManager.showConsoleView(console);
		return new PrintStream(console.newMessageStream());
	}

	private void displayError(String messageText) {
		openDialogWindow(messageText, "Pretty Printer Exception");
	}

	protected void openDialogWindow(String dialogText, String dialogTitle) {
		String[] buttonLabels = new String[] { IDialogConstants.OK_LABEL };
		MessageDialog dialog = new MessageDialog(getShell(), dialogTitle, null, dialogText,
				MessageDialog.INFORMATION, buttonLabels, 0);
		dialog.open();
	}

	protected Shell getShell() {
		return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
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
