import { expect, test } from "@playwright/test";

const board = (page) => page.locator(".board");

test("renders the playable board without overflow", async ({ page }, testInfo) => {
  await page.goto("/");

  await expect(page.getByRole("heading", { name: "2048" })).toBeVisible();
  await expect(board(page)).toBeVisible();
  await expect(page.getByText("Score").first()).toBeVisible();

  const box = await board(page).boundingBox();
  expect(box).not.toBeNull();
  expect(box!.width).toBeGreaterThan(280);
  expect(box!.width).toBeLessThanOrEqual(testInfo.project.use.viewport?.width ?? 1440);
});

test("moves with keyboard without focusing the board and can submit a ranked score", async ({ page }, testInfo) => {
  test.skip(testInfo.project.name.includes("mobile"), "Mobile play is covered by swipe input.");

  await page.goto("/");
  await expect(page.getByRole("button", { name: "Submit" }).first()).toBeDisabled();
  await page.locator("body").click({ position: { x: 12, y: 12 } });

  await page.keyboard.press("ArrowLeft");
  await page.keyboard.press("ArrowUp");
  await page.keyboard.press("ArrowRight");
  await page.keyboard.press("ArrowDown");
  await expect(page.getByRole("button", { name: "Submit" }).first()).toBeEnabled();

  await page.getByRole("button", { name: "Submit" }).first().click();
  await expect(page.locator(".notice")).toContainText(/Submitted|Submitting|Ranked|session|points/i);
});

test("does not move the board while typing in form controls", async ({ page }) => {
  await page.goto("/");

  await page.getByLabel("Nickname").fill("ArrowTester");
  await page.keyboard.press("ArrowLeft");

  await expect(page.getByRole("button", { name: "Submit" }).first()).toBeDisabled();
});

test("supports mobile swipe input", async ({ page }) => {
  await page.goto("/");

  const box = await board(page).boundingBox();
  expect(box).not.toBeNull();

  const startX = box!.x + box!.width * 0.75;
  const endX = box!.x + box!.width * 0.25;
  const y = box!.y + box!.height * 0.5;

  await page.mouse.move(startX, y);
  await page.mouse.down();
  await page.mouse.move(endX, y, { steps: 6 });
  await page.mouse.up();

  await expect(board(page).locator(".tile-value").first()).toBeVisible();
});

test("opens settings and starts a configured game", async ({ page }) => {
  await page.goto("/");

  await page.getByRole("button", { name: "Settings" }).click();
  await page.locator("select").first().selectOption("5");
  await page.getByRole("button", { name: "New" }).first().click();

  await expect(board(page)).toHaveClass(/board-size-5/);
});
