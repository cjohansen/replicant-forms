(ns toil.forms-test
  (:require [clojure.test :refer [deftest is testing]]
            [toil.forms :as forms]))

(deftest validate-form-test
  (testing "Validates required field"
    (is (= (forms/validate-form-data
            {:form/id :forms/test-form
             :form/fields
             [{:k :task/name
               :validations [{:validation/kind :required}]}]}
            {:task/name nil})
           [{:validation-error/field :task/name
             :validation-error/message "Please type in some text"}])))

  (testing "Empty strings do not satisfy requiredness"
    (is (= (forms/validate-form-data
            {:form/id :forms/test-form
             :form/fields
             [{:k :task/name
               :validations [{:validation/kind :required}]}]}
            {:task/name ""})
           [{:validation-error/field :task/name
             :validation-error/message "Please type in some text"}])))

  (testing "Validates required field with custom message"
    (is (= (forms/validate-form-data
            {:form/id :forms/test-form
             :form/fields
             [{:k :task/name
               :validations [{:validation/kind :required
                              :validation/message "Oh no!"}]}]}
            {:task/name nil})
           [{:validation-error/field :task/name
             :validation-error/message "Oh no!"}])))

  (testing "Passes validation for required field"
    (is (= (forms/validate-form-data
            {:form/id :forms/test-form
             :form/fields
             [{:k :task/name
               :validations [{:validation/kind :required
                              :validation/message "Oh no!"}]}]}
            {:task/name "I'm ok!"})
           [])))

  (testing "Validates max number field"
    (is (= (forms/validate-form-data
            {:form/id :forms/test-form
             :form/fields
             [{:k :task/duration
               :validations [{:validation/kind :max-num
                              :max 60}]}]}
            {:task/duration 65})
           [{:validation-error/field :task/duration
             :validation-error/message "Should be max 60"}])))

  (testing "Validates max number with custom message"
    (is (= (forms/validate-form-data
            {:form/id :forms/test-form
             :form/fields
             [{:k :task/duration
               :validations [{:validation/kind :max-num
                              :validation/message "I don't think so"
                              :max 60}]}]}
            {:task/duration 65})
           [{:validation-error/field :task/duration
             :validation-error/message "I don't think so"}])))

  (testing "Passes max validation"
    (is (= (forms/validate-form-data
            {:form/id :forms/test-form
             :form/fields
             [{:k :task/duration
               :validations [{:validation/kind :max-num
                              :max 60}]}]}
            {:task/duration 55})
           []))))

(deftest submit-form-test
  (testing "Validates form"
    (is (= (forms/submit
            {:form/id [:forms/test-form 1]
             :form/fields
             [{:k :task/name
               :validations [{:validation/kind :required}]}]}
            {:task/name nil}
            1)
           [[:db/transact
             [{:form/id [:forms/test-form 1]
               :form/validation-errors
               [{:validation-error/field :task/name
                 :validation-error/message "Please type in some text"}]}]]])))

  (testing "Calls form handle when form is valid"
    (is (= (forms/submit
            {:form/id [:forms/test-form 1]
             :form/handler (fn [data task-id]
                             [[:db/transact [(assoc data :db/id task-id)]]])}
            {:task/name "Do it!"}
            1)
           [[:db/transact
             [{:db/id 1
               :task/name "Do it!"}]]])))

  (testing "Cleans up form even when it doesn't do a db/transact"
    (is (= (forms/submit
            {:form/id [:forms/test-form 1]
             :form/handler (fn [_ _] [])}
            {:task/name "Do it!"}
            1)
           [[:db/transact
             [[:db/retractEntity [:form/id [:forms/test-form 1]]]]]]))))
