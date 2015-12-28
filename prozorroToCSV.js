var fs = require('fs');
var Promise = require('promise');
var stringify = require('csv-stringify');

var dir = './tenders/';
var tenders = [];

function readFiles(dirname, onFileContent, onError, allDone) {
  fs.readdir(dirname, function(err, filenames) {
    if (err) {
      onError(err);
      allDone();
      return;
    }
    var filesPromise = filenames.reduce(function(chain, filename) {
      return chain.then(function() {
        return new Promise(function(resolve) {
          fs.readFile(dirname + filename, 'utf-8', function(err, data) {
            onFileContent(filename, data);
            resolve();
          });
        });
      });
    }, Promise.resolve());
    filesPromise.then(function() {allDone();}, function(err) {onError();});
  });
}

function getRows(tender) {
  function getAward(award) {
    return [
      tender.id,
      tender.enquiryPeriod.startDate,
      tender.tenderPeriod.endDate,
      tender.procuringEntity.name,
      tender.procuringEntity.identifier.id,
      tender.procuringEntity.contactPoint.email,
      tender.procuringEntity.contactPoint.url,
      tender.title,
      award.lotID,
      award.title,
      award.suppliers[0].name,
      award.suppliers[0].identifier.id,
      award.value.amount,
      award.value.currency
    ];
  }
  var awards = tender.awards.filter(function(award) { return award.status == "active"; });
  return awards.map(getAward);
}

function saveCSV(filename, data) {
  data = data.filter(function(tender) { return tender.data.status == "complete"; });
  data = data.map(function(item) {
    return getRows(item.data);
  }).reduce(function(prev, next) {
    next.forEach(function(i) { prev.push(i); });
    return prev;
  }, []);
  data.unshift(["id", "start_date", "end_date",
                "customer_name", "customer_code", "customer_email", "customer_url",
                "subject", "lot_number", "lot_description",
                "winner_name", "winner_code",
                "price", "currency"])
  fs.open(filename, 'w', function(err, fd) {
    stringify(data, function(err, str) {
      fs.writeSync(fd, str);
    });
  });
}

var data = [];
readFiles('./tenders/', function(filename, content) {
  data.push(JSON.parse(content));
}, function(error) {
  throw error;
}, function done() {
  saveCSV('tenders_completed.csv', data);
});
