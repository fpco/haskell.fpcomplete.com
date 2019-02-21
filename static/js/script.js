var headings = document.querySelectorAll("h2[id], h3[id], h4[id], h5[id], h6[id]");

for (var i = 0; i < headings.length; i++) {
    var permalink = document.createElement("a");
    permalink.href = "#" + headings[i].id;
    permalink.classList.add("permalink");
    permalink.innerText = "¶";
    headings[i].appendChild(permalink);
}
