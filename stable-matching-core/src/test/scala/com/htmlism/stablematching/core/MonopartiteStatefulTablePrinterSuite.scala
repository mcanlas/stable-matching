package com.htmlism.stablematching.core

import weaver.*

object MonopartiteStatefulTablePrinterSuite extends FunSuite:
  test("prints"):
    matches(MonoFixtures.buildPopSixEmptyTable):
      case Right(table) =>
        println:
          MonopartiteStatefulTablePrinter
            .generateMarkdown(table)

        expect(true)
