document.getElementById("open-shirt").addEventListener("click", function () {
  for (const [id, value] of [["group", "All"], ["scene", "shirt"]]) {
    const select = document.getElementById(id);
    if (select.value !== value) {
      select.value = value;
      select.dispatchEvent(new Event("change", { bubbles: true }));
    }
  }
  document.querySelector('#view input[value="scenes"]').click();
});
