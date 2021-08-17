
const jspdf = require('jspdf')
const html2canvas = require('html2canvas')



exports.printProofer = () => {
    html2canvas(document.getElementById("printable")).then(function(canvas) {
        document.body.appendChild(canvas);
        var imgdata = canvas.toDataURL("image/png");
        var imgWidth = 105;
        var pageHeight = 295;
        var imgHeight = canvas.height * imgWidth / canvas.width;
        var heightLeft = imgHeight;
        var position = 5;

        var doc = new jspdf.jsPDF();
        doc.addImage(imgdata, "PNG", 52.5, position, imgWidth, imgHeight);
        heightLeft -= pageHeight;
        
        while (heightLeft >= 0) {
            position = heightLeft - imgHeight;
            doc.addPage();
            doc.addImage(imgdata, "PNG", 52, position, imgWidth, imgHeight);
            heightLeft -= pageHeight;
        }

        doc.save("proof.pdf");
    });
}

