export function AppHeader({
	onRunButtonClick,
	onSaveButtonClick,
}: { onRunButtonClick?: () => void; onSaveButtonClick?: () => void }) {
	return (
		<header
			css={{
				display: "flex",
				flexDirection: "row",
				alignItems: "center",
				justifyContent: "space-between",
				gap: 16,
				padding: "0 32px",
				height: 48,
				background: "#da7b2d",
			}}
		>
			<h1 css={{ padding: 0, fontSize: "1.5rem", color: "#fff" }}>
				Empitsu Playground
			</h1>
			<div
				css={{
					padding: 0,
					display: "flex",
					flexDirection: "row",
					gap: 16,
				}}
			>
				<button onClick={onRunButtonClick} type="button">
					Run
				</button>
				<button onClick={onSaveButtonClick} type="button">
					Save
				</button>
			</div>
		</header>
	);
}
