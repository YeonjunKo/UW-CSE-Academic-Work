/*
 * Copyright ©2024 Hal Perkins.  All rights reserved.  Permission is
 * hereby granted to students registered for University of Washington
 * CSE 333 for use solely during Winter Quarter 2024 for purposes of
 * the course.  No other use, copying, distribution, or modification
 * is permitted without prior written consent. Copyrights for
 * third-party components of this work must be honored.  Instructors
 * interested in reusing these course materials should contact the
 * author.
 */

#include "./QueryProcessor.h"

#include <iostream>
#include <algorithm>
#include <list>
#include <string>
#include <vector>
#include <map>

extern "C" {
  #include "./libhw1/CSE333.h"
}

using std::list;
using std::sort;
using std::string;
using std::vector;

namespace hw3 {

QueryProcessor::QueryProcessor(const list<string>& index_list, bool validate) {
  // Stash away a copy of the index list.
  index_list_ = index_list;
  array_len_ = index_list_.size();
  Verify333(array_len_ > 0);

  // Create the arrays of DocTableReader*'s. and IndexTableReader*'s.
  dtr_array_ = new DocTableReader* [array_len_];
  itr_array_ = new IndexTableReader* [array_len_];

  // Populate the arrays with heap-allocated DocTableReader and
  // IndexTableReader object instances.
  list<string>::const_iterator idx_iterator = index_list_.begin();
  for (int i = 0; i < array_len_; i++) {
    FileIndexReader fir(*idx_iterator, validate);
    dtr_array_[i] = fir.NewDocTableReader();
    itr_array_[i] = fir.NewIndexTableReader();
    idx_iterator++;
  }
}

QueryProcessor::~QueryProcessor() {
  // Delete the heap-allocated DocTableReader and IndexTableReader
  // object instances.
  Verify333(dtr_array_ != nullptr);
  Verify333(itr_array_ != nullptr);
  for (int i = 0; i < array_len_; i++) {
    delete dtr_array_[i];
    delete itr_array_[i];
  }

  // Delete the arrays of DocTableReader*'s and IndexTableReader*'s.
  delete[] dtr_array_;
  delete[] itr_array_;
  dtr_array_ = nullptr;
  itr_array_ = nullptr;
}

// This structure is used to store a index-file-specific query result.
typedef struct {
  DocID_t doc_id;  // The document ID within the index file.
  int     rank;    // The rank of the result so far.
} IdxQueryResult;

vector<QueryProcessor::QueryResult>
QueryProcessor::ProcessQuery(const vector<string>& query) const {
  Verify333(query.size() > 0);

  // STEP 1.
  // (the only step in this file)
  vector<QueryProcessor::QueryResult> final_result;
  int query_size = query.size();
  int i, j;
  string file_name;
  DocIDTableReader *dr;
  list<DocIDElementHeader> docIdList;
  std::map<string, int> common_hash;
  std::map<string, int>::iterator it;

  // Find query result for the first word
  for (j = 0; j < array_len_; j++) {
    dr = itr_array_[j]->LookupWord(query[0]);
    if (dr == nullptr) {
      continue;
    }

    docIdList = dr->GetDocIDList();
    for (auto const &docId : docIdList) {
      dtr_array_[j]->LookupDocID(docId.doc_id, &file_name);
      common_hash[file_name] = docId.num_positions;
    }

    delete dr;
  }

  if (query_size > 1) {
    for (i = 1; i < query_size; i++) {
      // find common doc files with previous searched result
      std::map<string, int> tmp_hash;
      for (j = 0; j < array_len_; j++) {
        dr = itr_array_[j]->LookupWord(query[i]);
        if (dr == nullptr) {
          continue;
        }

        docIdList = dr->GetDocIDList();
        for (auto const &docId : docIdList) {
          dtr_array_[j]->LookupDocID(docId.doc_id, &file_name);

          it = common_hash.find(file_name);
          if (it != common_hash.end()) {
            tmp_hash[file_name] = it->second + docId.num_positions;
          }
        }

        delete dr;
      }

      if (tmp_hash.empty()) {
        // there's no common set - returns empty list
        return final_result;
      }

      // update the doc files hash.
      common_hash.clear();
      common_hash.insert(tmp_hash.begin(), tmp_hash.end());
    }
  }

  // copy to the final result
  for (it = common_hash.begin(); it != common_hash.end(); ++it) {
    QueryProcessor::QueryResult query_result;
    query_result.document_name = it->first;
    query_result.rank = it->second;
    final_result.push_back(query_result);
  }

  // Sort the final results.
  sort(final_result.begin(), final_result.end());
  return final_result;
}

}  // namespace hw3
