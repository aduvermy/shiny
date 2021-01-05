function filterTable() {

  i.value = Number(i.value) + 1;
}

function filterTable(tab) {
    let i = document.getElementsByTagName("input")[0];
    //var srch = document.getElementById('res-filter');
    //var val = srch.value.toLowerCase();
    //var valArr = val.split(' ');
    //var tbl = document.getElementById('res-table');
    let tblLength = tab.rows.length;
    console.log(tblLength)
    //if (tblLength != 0) {
    //    for (var i = 1; i < tblLength; i++) {
    //        tbl.rows[i].style.display = 'table-row';
    //        for (var j = 0; j < valArr.length; j++) {
      //          if (tbl.rows[i].textContent.toLowerCase().indexOf(valArr[j]) === -1) {
        //            tbl.rows[i].style.display = 'none';
                    //
              //}
            //}
        //}
    //}
}



let table = document.querySelector("table[id='domains']")
table.addEventListener('keyup', filterTable);
