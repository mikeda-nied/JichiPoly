var datasets = {};
var f1 = function(e) { 
  e.preventDefault();
};
var f2 = function(e) {
    e.preventDefault();
    
    var file = e.dataTransfer.files[0];
    
    var reader = new FileReader();
    reader.name = file.name;
    
    reader.onload = function(e) {
        datasets[e.target.name] = e.target.result;
        Shiny.onInputChange("datasets", datasets);
    };
    reader.readAsText(file);
};