/**
Turn on the Google API (only need to do this once)

1] In the Apps Script editor, click Resources > Advanced Google Services.
2] Locate Sheets API in the dialog and click the corresponding toggle, setting it to on.
3] Click the Google API Console link.
  or Enter " DCM/DFA Reporting and Trafficking API" into the search box and click on the corresponding entry in the results.
4] Click the Enable API button.
5] Return to the Apps Script editor and click the OK button on the Advanced Google Services dialog.
*/

//Trigger to activate the menu
function createSpreadsheetOpenTrigger() {
  var ss = SpreadsheetApp.getActive();
  ScriptApp.newTrigger('createSpreadsheetMenu')
      .forSpreadsheet(ss)
      .onOpen()
      .create();
}

//Function to create the menu to be displayed at Spreadsheet
function createSpreadsheetMenu () {
  var ui = SpreadsheetApp.getUi();
  ui.createMenu('DBM')
      .addItem('BidManager', 'DCMfetcher')
      .addToUi();
}

// DoubleClick Bid Manager Report Fetcher
function DCMfetcher() {
 var ss = SpreadsheetApp.openById('your_spreadsheet_id');
  
 var parametersSheet = ss.getSheetByName('PARAMETERS');
  
  // Fetch the range of cells B2:B10
 var parametersDataRange = parametersSheet.getRange(2, 1, 9, 2); 
 
 // Fetch cell value for each row in the range.
 var parametersData = parametersDataRange.getValues()
  gSheetID1 = parametersData[0][1];
  gSheetID2 = parametersData[1][1];
  gSheetID3 = parametersData[2][1];

  var profileId = gSheetID1;
  var reportId = gSheetID2;
  var fileId = gSheetID3;
  
  var reportUrl =  DoubleClickCampaigns.Files.get(reportId, fileId).urls.apiUrl;
  
  var token = ScriptApp.getOAuthToken();


  var headersOptions = { 
    Authorization : 'Bearer '  + token
    };


  var options = { 
    headers : headersOptions,
    muteHttpExceptions : false
    };


  var csvDoc = UrlFetchApp.fetch(reportUrl, options);
  
  var csvData = Utilities.parseCsv(csvDoc);
  
  var sheet = SpreadsheetApp.openById('your_spreadsheet_id').getSheetByName('Data');
  sheet.getRange(1, 1, csvData.length, csvData[0].length).setValues(csvData);
  
}
