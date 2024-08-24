import * as vm from "empitsu-core-wasm";
import * as monaco from "monaco-editor";
import { useCallback, useEffect, useRef, useState } from "react";
import { AppHeader } from "./AppHeader";
import { MonacoEditor } from "./MonacoEditor";

function encodeBase64(value: string): string {
	return btoa(value);
}

function decodeBase64(value: string): string {
	return atob(value);
}

function useModel() {
	const [model, _setModel] = useState(() => {
		const savedCode = decodeBase64(
			new URL(window.location.href).searchParams.get("code") ?? "",
		);

		return monaco.editor.createModel(savedCode);
	});
	const [output, setOutput] = useState("");

	const save = useCallback(() => {
		const value = model.getValue();
		const url = new URL(window.location.href);
		url.searchParams.set("code", encodeBase64(value));
		history.replaceState("", "", url.toString());
	}, [model]);

	const run = useCallback(() => {
		save();

		const input = model.getValue();
		const date = new Date();
		const output = evaluate(input);

		setOutput((oldOutput) =>
			`${oldOutput}\n\n[${date.toISOString()}]\n${output}`.trim(),
		);
	}, [model, save]);

	useEffect(() => {
		monaco.editor.addCommand({
			id: "save",
			run: () => save(),
		});
		monaco.editor.addCommand({
			id: "run",
			run: () => run(),
		});
		monaco.editor.addKeybindingRules([
			{
				keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS,
				command: "save",
				when: "textInputFocus",
			},
			{
				keybinding: monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter,
				command: "run",
				when: "textInputFocus",
			},
		]);
	}, [save, run]);

	return { model, output, save, run };
}

function evaluate(input: string): string {
	return vm.evaluate(input);
}

export const App = () => {
	const { model, output, save, run } = useModel();

	return (
		<div
			css={{
				position: "fixed",
				inset: 0,
				display: "grid",
				gridTemplate: `"header header" min-content
			"editor output" 1fr / 50% 50%`,
			}}
		>
			<div css={{ gridArea: "header" }}>
				<AppHeader onRunButtonClick={run} onSaveButtonClick={save} />
			</div>
			<div
				css={{
					gridArea: "editor",
					position: "relative",
					overflow: "hidden",
					borderRight: "1px solid #c0c0c0",
				}}
			>
				<MonacoEditor model={model} />
			</div>
			<div css={{ gridArea: "output", position: "relative", fontSize: 18 }}>
				<pre
					css={{
						position: "absolute",
						inset: 0,
						padding: 16,
						whiteSpace: "pre-wrap",
						fontFamily: "monospace",
						lineHeight: 1.5,
						overflow: "auto",
					}}
				>
					{output}
				</pre>
			</div>
		</div>
	);
};
