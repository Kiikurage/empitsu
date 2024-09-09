import * as vm from "empitsu-core-wasm";
import * as monaco from "monaco-editor";
import { type ReactNode, useCallback, useEffect, useState } from "react";
import { AppHeader } from "./AppHeader";
import { MonacoEditor } from "./MonacoEditor";

function encodeBase64(value: string): string {
	return btoa(encodeURIComponent(value));
}

function decodeBase64(value: string): string {
	return decodeURIComponent(atob(value));
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
	const [showManual, setShowManual] = useState(false);

	return (
		<div
			css={{
				position: "fixed",
				inset: 0,
				display: "grid",
				gridTemplate: `"header header header" min-content
			"editor output help" 1fr / 1fr 1fr min-content`,
			}}
		>
			<div css={{ gridArea: "header" }}>
				<AppHeader
					onRunButtonClick={run}
					onSaveButtonClick={save}
					onHelpButtonClick={() => setShowManual((oldValue) => !oldValue)}
				/>
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
			<div css={{ gridArea: "help", overflow: "auto" }}>
				{showManual && <Manual />}
			</div>
		</div>
	);
};

function Manual() {
	return (
		<div
			css={{
				position: "relative",
				fontSize: 18,
				width: 400,
				background: "#f0f0f0",
				padding: 16,
				display: "flex",
				flexDirection: "column",
				gap: 32,
				alignItems: "stretch",
				justifyContent: "flex-start",

				h2: {
					fontSize: "1.5rem",
					margin: 0,
				},

				h3: {
					fontSize: "1rem",
					fontWeight: "normal",
					margin: 0,
				},
			}}
		>
			<section>
				<h2>If文・If式</h2>
				<Code>
					{`
if (条件式) {
  /* 条件式がtrueの場合の処理 */
} else {
  /* 条件式がfalseの場合の処理 */
}
				`.trim()}
				</Code>
			</section>
			<section>
				<h2>For文</h2>
				<Code>
					{`
for (i in 0 to 5) {
  /* 繰り返す処理 */
}
				`.trim()}
				</Code>
			</section>
			<section>
				<h2>変数</h2>
				<Code>
					{`
let x:number = 42

// 型を省略した場合は初期値から推論される
let y = 42

// 初期値の省略も可能
// 初期化前にアクセスするとエラーとなる
let z
test(z)  // => error
				`.trim()}
				</Code>
			</section>
			<section>
				<h2>関数</h2>
				<h3>定義</h3>
				<Code>
					{`
fn double(x: number): number {
  x * 2  // 最後の式の評価値が返り値になる
}
				`.trim()}
				</Code>
				<h3>呼び出し</h3>
				<Code>
					{`
let y = double(21)  // => 42

// 名前付きでの指定も可能
let z = double(x=21)
				`.trim()}
				</Code>
			</section>
			<section>
				<h2>構造体</h2>

				<h3>定義</h3>
				<Code>
					{`
struct User(
  id: number,
  name: string
) {
  fn getName(self): string {
    return self.name;
  }
}
                 	`.trim()}
				</Code>

				<h3>インスタンスの利用</h3>
				<Code>
					{`
let user = User(id=1, name="Alice");
user.getName()  // => "Alice"
                 	`.trim()}
				</Code>
			</section>
			<section>
				<h2>インターフェース</h2>

				<h3>定義</h3>
				<Code>
					{`
interface Printable {
  fn print(self): string
}
                 	`.trim()}
				</Code>

				<h3>型としての利用</h3>
				<Code>
					{`
fn stringify(value: Printable): string {
  value.print()
}
                 	`.trim()}
				</Code>

				<h3>既存の構造体への実装</h3>
				<Code>
					{`
struct User(id: number, name: string)

impl Printable for User {
  fn print(self): string {
	return self.name
  }
}

let user = User(id=1, name="Alice")
stringify(user)  // => "Alice"
                 	`.trim()}
				</Code>
			</section>
			<section>
				<h2>演算子</h2>
				<Code>
					{`
// 算術演算
1 + 2
1 - 2
1 * 2
1 / 2

// 比較演算
1 == 2
1 != 2
1 < 2
1 <= 2
1 > 2
1 >= 2

// 論理演算
!flag
flag1 && flag2
flag1 || flag2
                 	`.trim()}
				</Code>
			</section>
			<section>
				<h2>型</h2>

				<h3>組み込み型</h3>
				<Code>
					{`
let x: number = 123
let y: string = "hello"
let z: boolean = true
				 	`.trim()}
				</Code>

				<h3>Optional型</h3>
				<Code>
					{`
let x: number? = 123
				 	`.trim()}
				</Code>

				<h3>Union型</h3>
				<Code>
					{`
fn test1(value: number|string): string {
	// ...
}

fn test2(value: (number|string)?): string {
	// ...
}
				 	`.trim()}
				</Code>
			</section>
		</div>
	);
}

function Code({ children }: { children?: ReactNode }) {
	return (
		<code
			css={{
				fontSize: "0.875rem",
				display: "block",
				padding: "8px 16px",
				margin: "8px 0",
				borderRadius: 4,
				background: "#d8d8d8",
				color: "#30333c",
			}}
		>
			<pre css={{ margin: 0 }}>{children}</pre>
		</code>
	);
}
