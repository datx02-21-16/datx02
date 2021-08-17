
const jspdf = require('jspdf')
const html2canvas = require('html2canvas')



exports.printProofer = () => {
    html2canvas(document.getElementById("printable"), {scale:0.5}).then(function(canvas) {
        document.body.appendChild(canvas);
        var imgdata = canvas.toDataURL("image/png");
        var doc = new jspdf.jsPDF();
        var width = doc.internal.pageSize.getWidth();    
        var height = doc.internal.pageSize.getHeight();
        doc.addImage(imgdata, "PNG", 5, 5);
        doc.save("proof.pdf");
    });
}

