(program
  (let
    (nonrec)
    (datatypebind
      (datatype
        (tyvardecl Ordering (type))

        Ordering_match
        (vardecl EQ Ordering) (vardecl GT Ordering) (vardecl LT Ordering)
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Bool (type))

        Bool_match
        (vardecl True Bool) (vardecl False Bool)
      )
    )
    (termbind
      (strict)
      (vardecl
        fOrdInteger_ccompare (fun (con integer) (fun (con integer) Ordering))
      )
      (lam
        x
        (con integer)
        (lam
          y
          (con integer)
          {
            [
              [
                {
                  [
                    Bool_match
                    [
                      [
                        [
                          { (builtin ifThenElse) Bool }
                          [ [ (builtin equalsInteger) x ] y ]
                        ]
                        True
                      ]
                      False
                    ]
                  ]
                  (all dead (type) Ordering)
                }
                (abs dead (type) EQ)
              ]
              (abs
                dead
                (type)
                {
                  [
                    [
                      {
                        [
                          Bool_match
                          [
                            [
                              [
                                { (builtin ifThenElse) Bool }
                                [ [ (builtin lessThanEqualsInteger) x ] y ]
                              ]
                              True
                            ]
                            False
                          ]
                        ]
                        (all dead (type) Ordering)
                      }
                      (abs dead (type) LT)
                    ]
                    (abs dead (type) GT)
                  ]
                  (all dead (type) dead)
                }
              )
            ]
            (all dead (type) dead)
          }
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Extended (fun (type) (type)))
        (tyvardecl a (type))
        Extended_match
        (vardecl Finite (fun a [Extended a]))
        (vardecl NegInf [Extended a])
        (vardecl PosInf [Extended a])
      )
    )
    (termbind
      (strict)
      (vardecl
        wsfOrdUpperBound0_c
        (fun [Extended (con integer)] (fun Bool (fun [Extended (con integer)] (fun Bool Bool))))
      )
      (lam
        ww
        [Extended (con integer)]
        (lam
          ww
          Bool
          (lam
            ww
            [Extended (con integer)]
            (lam
              ww
              Bool
              {
                [
                  [
                    [
                      {
                        [ { Extended_match (con integer) } ww ]
                        (all dead (type) Bool)
                      }
                      (lam
                        default_arg0
                        (con integer)
                        (abs
                          dead
                          (type)
                          {
                            [
                              [
                                [
                                  {
                                    [ { Extended_match (con integer) } ww ]
                                    (all dead (type) Bool)
                                  }
                                  (lam
                                    default_arg0
                                    (con integer)
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [
                                                  {
                                                    Extended_match (con integer)
                                                  }
                                                  ww
                                                ]
                                                (all dead (type) Bool)
                                              }
                                              (lam
                                                ipv
                                                (con integer)
                                                (abs
                                                  dead
                                                  (type)
                                                  {
                                                    [
                                                      [
                                                        [
                                                          {
                                                            [
                                                              {
                                                                Extended_match
                                                                (con integer)
                                                              }
                                                              ww
                                                            ]
                                                            (all dead (type) Bool)
                                                          }
                                                          (lam
                                                            ipv
                                                            (con integer)
                                                            (abs
                                                              dead
                                                              (type)
                                                              {
                                                                [
                                                                  [
                                                                    [
                                                                      {
                                                                        [
                                                                          Ordering_match
                                                                          [
                                                                            [
                                                                              fOrdInteger_ccompare
                                                                              ipv
                                                                            ]
                                                                            ipv
                                                                          ]
                                                                        ]
                                                                        (all dead (type) Bool)
                                                                      }
                                                                      (abs
                                                                        dead
                                                                        (type)
                                                                        {
                                                                          [
                                                                            [
                                                                              {
                                                                                [
                                                                                  Bool_match
                                                                                  ww
                                                                                ]
                                                                                (all dead (type) Bool)
                                                                              }
                                                                              (abs
                                                                                dead
                                                                                (type)
                                                                                ww
                                                                              )
                                                                            ]
                                                                            (abs
                                                                              dead
                                                                              (type)
                                                                              True
                                                                            )
                                                                          ]
                                                                          (all dead (type) dead)
                                                                        }
                                                                      )
                                                                    ]
                                                                    (abs
                                                                      dead
                                                                      (type)
                                                                      False
                                                                    )
                                                                  ]
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    True
                                                                  )
                                                                ]
                                                                (all dead (type) dead)
                                                              }
                                                            )
                                                          )
                                                        ]
                                                        (abs
                                                          dead
                                                          (type)
                                                          (error Bool)
                                                        )
                                                      ]
                                                      (abs dead (type) True)
                                                    ]
                                                    (all dead (type) dead)
                                                  }
                                                )
                                              )
                                            ]
                                            (abs dead (type) (error Bool))
                                          ]
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Extended_match
                                                          (con integer)
                                                        }
                                                        ww
                                                      ]
                                                      (all dead (type) Bool)
                                                    }
                                                    (lam
                                                      ipv
                                                      (con integer)
                                                      (abs dead (type) False)
                                                    )
                                                  ]
                                                  (abs dead (type) (error Bool))
                                                ]
                                                (abs
                                                  dead
                                                  (type)
                                                  {
                                                    [
                                                      [
                                                        {
                                                          [ Bool_match ww ]
                                                          (all dead (type) Bool)
                                                        }
                                                        (abs dead (type) ww)
                                                      ]
                                                      (abs dead (type) True)
                                                    ]
                                                    (all dead (type) dead)
                                                  }
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  )
                                ]
                                (abs dead (type) False)
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [
                                            { Extended_match (con integer) } ww
                                          ]
                                          (all dead (type) Bool)
                                        }
                                        (lam
                                          ipv
                                          (con integer)
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Extended_match
                                                          (con integer)
                                                        }
                                                        ww
                                                      ]
                                                      (all dead (type) Bool)
                                                    }
                                                    (lam
                                                      ipv
                                                      (con integer)
                                                      (abs
                                                        dead
                                                        (type)
                                                        {
                                                          [
                                                            [
                                                              [
                                                                {
                                                                  [
                                                                    Ordering_match
                                                                    [
                                                                      [
                                                                        fOrdInteger_ccompare
                                                                        ipv
                                                                      ]
                                                                      ipv
                                                                    ]
                                                                  ]
                                                                  (all dead (type) Bool)
                                                                }
                                                                (abs
                                                                  dead
                                                                  (type)
                                                                  {
                                                                    [
                                                                      [
                                                                        {
                                                                          [
                                                                            Bool_match
                                                                            ww
                                                                          ]
                                                                          (all dead (type) Bool)
                                                                        }
                                                                        (abs
                                                                          dead
                                                                          (type)
                                                                          ww
                                                                        )
                                                                      ]
                                                                      (abs
                                                                        dead
                                                                        (type)
                                                                        True
                                                                      )
                                                                    ]
                                                                    (all dead (type) dead)
                                                                  }
                                                                )
                                                              ]
                                                              (abs
                                                                dead
                                                                (type)
                                                                False
                                                              )
                                                            ]
                                                            (abs
                                                              dead (type) True
                                                            )
                                                          ]
                                                          (all dead (type) dead)
                                                        }
                                                      )
                                                    )
                                                  ]
                                                  (abs dead (type) (error Bool))
                                                ]
                                                (abs dead (type) True)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) (error Bool))
                                    ]
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [
                                                  {
                                                    Extended_match (con integer)
                                                  }
                                                  ww
                                                ]
                                                (all dead (type) Bool)
                                              }
                                              (lam
                                                ipv
                                                (con integer)
                                                (abs dead (type) False)
                                              )
                                            ]
                                            (abs dead (type) (error Bool))
                                          ]
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  {
                                                    [ Bool_match ww ]
                                                    (all dead (type) Bool)
                                                  }
                                                  (abs dead (type) ww)
                                                ]
                                                (abs dead (type) True)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (all dead (type) dead)
                          }
                        )
                      )
                    ]
                    (abs
                      dead
                      (type)
                      {
                        [
                          [
                            [
                              {
                                [ { Extended_match (con integer) } ww ]
                                (all dead (type) Bool)
                              }
                              (lam
                                default_arg0
                                (con integer)
                                (abs dead (type) True)
                              )
                            ]
                            (abs
                              dead
                              (type)
                              {
                                [
                                  [
                                    { [ Bool_match ww ] (all dead (type) Bool) }
                                    (abs dead (type) ww)
                                  ]
                                  (abs dead (type) True)
                                ]
                                (all dead (type) dead)
                              }
                            )
                          ]
                          (abs dead (type) True)
                        ]
                        (all dead (type) dead)
                      }
                    )
                  ]
                  (abs
                    dead
                    (type)
                    {
                      [
                        [
                          [
                            {
                              [ { Extended_match (con integer) } ww ]
                              (all dead (type) Bool)
                            }
                            (lam
                              default_arg0
                              (con integer)
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [
                                            { Extended_match (con integer) } ww
                                          ]
                                          (all dead (type) Bool)
                                        }
                                        (lam
                                          ipv
                                          (con integer)
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Extended_match
                                                          (con integer)
                                                        }
                                                        ww
                                                      ]
                                                      (all dead (type) Bool)
                                                    }
                                                    (lam
                                                      ipv
                                                      (con integer)
                                                      (abs
                                                        dead
                                                        (type)
                                                        {
                                                          [
                                                            [
                                                              [
                                                                {
                                                                  [
                                                                    Ordering_match
                                                                    [
                                                                      [
                                                                        fOrdInteger_ccompare
                                                                        ipv
                                                                      ]
                                                                      ipv
                                                                    ]
                                                                  ]
                                                                  (all dead (type) Bool)
                                                                }
                                                                (abs
                                                                  dead
                                                                  (type)
                                                                  {
                                                                    [
                                                                      [
                                                                        {
                                                                          [
                                                                            Bool_match
                                                                            ww
                                                                          ]
                                                                          (all dead (type) Bool)
                                                                        }
                                                                        (abs
                                                                          dead
                                                                          (type)
                                                                          ww
                                                                        )
                                                                      ]
                                                                      (abs
                                                                        dead
                                                                        (type)
                                                                        True
                                                                      )
                                                                    ]
                                                                    (all dead (type) dead)
                                                                  }
                                                                )
                                                              ]
                                                              (abs
                                                                dead
                                                                (type)
                                                                False
                                                              )
                                                            ]
                                                            (abs
                                                              dead (type) True
                                                            )
                                                          ]
                                                          (all dead (type) dead)
                                                        }
                                                      )
                                                    )
                                                  ]
                                                  (abs dead (type) (error Bool))
                                                ]
                                                (abs dead (type) True)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) (error Bool))
                                    ]
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [
                                                  {
                                                    Extended_match (con integer)
                                                  }
                                                  ww
                                                ]
                                                (all dead (type) Bool)
                                              }
                                              (lam
                                                ipv
                                                (con integer)
                                                (abs dead (type) False)
                                              )
                                            ]
                                            (abs dead (type) (error Bool))
                                          ]
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  {
                                                    [ Bool_match ww ]
                                                    (all dead (type) Bool)
                                                  }
                                                  (abs dead (type) ww)
                                                ]
                                                (abs dead (type) True)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            )
                          ]
                          (abs dead (type) False)
                        ]
                        (abs
                          dead
                          (type)
                          {
                            [
                              [
                                [
                                  {
                                    [ { Extended_match (con integer) } ww ]
                                    (all dead (type) Bool)
                                  }
                                  (lam
                                    ipv
                                    (con integer)
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [
                                                  {
                                                    Extended_match (con integer)
                                                  }
                                                  ww
                                                ]
                                                (all dead (type) Bool)
                                              }
                                              (lam
                                                ipv
                                                (con integer)
                                                (abs
                                                  dead
                                                  (type)
                                                  {
                                                    [
                                                      [
                                                        [
                                                          {
                                                            [
                                                              Ordering_match
                                                              [
                                                                [
                                                                  fOrdInteger_ccompare
                                                                  ipv
                                                                ]
                                                                ipv
                                                              ]
                                                            ]
                                                            (all dead (type) Bool)
                                                          }
                                                          (abs
                                                            dead
                                                            (type)
                                                            {
                                                              [
                                                                [
                                                                  {
                                                                    [
                                                                      Bool_match
                                                                      ww
                                                                    ]
                                                                    (all dead (type) Bool)
                                                                  }
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    ww
                                                                  )
                                                                ]
                                                                (abs
                                                                  dead
                                                                  (type)
                                                                  True
                                                                )
                                                              ]
                                                              (all dead (type) dead)
                                                            }
                                                          )
                                                        ]
                                                        (abs dead (type) False)
                                                      ]
                                                      (abs dead (type) True)
                                                    ]
                                                    (all dead (type) dead)
                                                  }
                                                )
                                              )
                                            ]
                                            (abs dead (type) (error Bool))
                                          ]
                                          (abs dead (type) True)
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  )
                                ]
                                (abs dead (type) (error Bool))
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [
                                            { Extended_match (con integer) } ww
                                          ]
                                          (all dead (type) Bool)
                                        }
                                        (lam
                                          ipv
                                          (con integer)
                                          (abs dead (type) False)
                                        )
                                      ]
                                      (abs dead (type) (error Bool))
                                    ]
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            {
                                              [ Bool_match ww ]
                                              (all dead (type) Bool)
                                            }
                                            (abs dead (type) ww)
                                          ]
                                          (abs dead (type) True)
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (all dead (type) dead)
                          }
                        )
                      ]
                      (all dead (type) dead)
                    }
                  )
                ]
                (all dead (type) dead)
              }
            )
          )
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl UpperBound (fun (type) (type)))
        (tyvardecl a (type))
        UpperBound_match
        (vardecl UpperBound (fun [Extended a] (fun Bool [UpperBound a])))
      )
    )
    (termbind
      (strict)
      (vardecl
        sfOrdUpperBound0_c
        (fun [UpperBound (con integer)] (fun [UpperBound (con integer)] Bool))
      )
      (lam
        w
        [UpperBound (con integer)]
        (lam
          w
          [UpperBound (con integer)]
          [
            { [ { UpperBound_match (con integer) } w ] Bool }
            (lam
              ww
              [Extended (con integer)]
              (lam
                ww
                Bool
                [
                  { [ { UpperBound_match (con integer) } w ] Bool }
                  (lam
                    ww
                    [Extended (con integer)]
                    (lam ww Bool [ [ [ [ wsfOrdUpperBound0_c ww ] ww ] ww ] ww ]
                    )
                  )
                ]
              )
            )
          ]
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        wscontains
        (fun [Extended (con integer)] (fun Bool (fun [UpperBound (con integer)] (fun [Extended (con integer)] (fun Bool (fun [UpperBound (con integer)] Bool))))))
      )
      (lam
        ww
        [Extended (con integer)]
        (lam
          ww
          Bool
          (lam
            ww
            [UpperBound (con integer)]
            (lam
              ww
              [Extended (con integer)]
              (lam
                ww
                Bool
                (lam
                  ww
                  [UpperBound (con integer)]
                  (let
                    (nonrec)
                    (termbind
                      (nonstrict)
                      (vardecl j Bool)
                      {
                        [
                          [
                            { [ Bool_match ww ] (all dead (type) Bool) }
                            (abs
                              dead
                              (type)
                              {
                                [
                                  [
                                    { [ Bool_match ww ] (all dead (type) Bool) }
                                    (abs
                                      dead
                                      (type)
                                      [ [ sfOrdUpperBound0_c ww ] ww ]
                                    )
                                  ]
                                  (abs dead (type) False)
                                ]
                                (all dead (type) dead)
                              }
                            )
                          ]
                          (abs dead (type) [ [ sfOrdUpperBound0_c ww ] ww ])
                        ]
                        (all dead (type) dead)
                      }
                    )
                    {
                      [
                        [
                          [
                            {
                              [ { Extended_match (con integer) } ww ]
                              (all dead (type) Bool)
                            }
                            (lam
                              default_arg0
                              (con integer)
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [
                                            { Extended_match (con integer) } ww
                                          ]
                                          (all dead (type) Bool)
                                        }
                                        (lam
                                          default_arg0
                                          (con integer)
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Extended_match
                                                          (con integer)
                                                        }
                                                        ww
                                                      ]
                                                      (all dead (type) Bool)
                                                    }
                                                    (lam
                                                      ipv
                                                      (con integer)
                                                      (abs
                                                        dead
                                                        (type)
                                                        {
                                                          [
                                                            [
                                                              [
                                                                {
                                                                  [
                                                                    {
                                                                      Extended_match
                                                                      (con integer)
                                                                    }
                                                                    ww
                                                                  ]
                                                                  (all dead (type) Bool)
                                                                }
                                                                (lam
                                                                  ipv
                                                                  (con integer)
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    {
                                                                      [
                                                                        [
                                                                          [
                                                                            {
                                                                              [
                                                                                Ordering_match
                                                                                [
                                                                                  [
                                                                                    fOrdInteger_ccompare
                                                                                    ipv
                                                                                  ]
                                                                                  ipv
                                                                                ]
                                                                              ]
                                                                              (all dead (type) Bool)
                                                                            }
                                                                            (abs
                                                                              dead
                                                                              (type)
                                                                              j
                                                                            )
                                                                          ]
                                                                          (abs
                                                                            dead
                                                                            (type)
                                                                            False
                                                                          )
                                                                        ]
                                                                        (abs
                                                                          dead
                                                                          (type)
                                                                          [
                                                                            [
                                                                              sfOrdUpperBound0_c
                                                                              ww
                                                                            ]
                                                                            ww
                                                                          ]
                                                                        )
                                                                      ]
                                                                      (all dead (type) dead)
                                                                    }
                                                                  )
                                                                )
                                                              ]
                                                              (abs
                                                                dead
                                                                (type)
                                                                (error Bool)
                                                              )
                                                            ]
                                                            (abs
                                                              dead
                                                              (type)
                                                              [
                                                                [
                                                                  sfOrdUpperBound0_c
                                                                  ww
                                                                ]
                                                                ww
                                                              ]
                                                            )
                                                          ]
                                                          (all dead (type) dead)
                                                        }
                                                      )
                                                    )
                                                  ]
                                                  (abs dead (type) (error Bool))
                                                ]
                                                (abs
                                                  dead
                                                  (type)
                                                  {
                                                    [
                                                      [
                                                        [
                                                          {
                                                            [
                                                              {
                                                                Extended_match
                                                                (con integer)
                                                              }
                                                              ww
                                                            ]
                                                            (all dead (type) Bool)
                                                          }
                                                          (lam
                                                            ipv
                                                            (con integer)
                                                            (abs
                                                              dead (type) False
                                                            )
                                                          )
                                                        ]
                                                        (abs
                                                          dead
                                                          (type)
                                                          (error Bool)
                                                        )
                                                      ]
                                                      (abs dead (type) j)
                                                    ]
                                                    (all dead (type) dead)
                                                  }
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) False)
                                    ]
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [
                                                  {
                                                    Extended_match (con integer)
                                                  }
                                                  ww
                                                ]
                                                (all dead (type) Bool)
                                              }
                                              (lam
                                                ipv
                                                (con integer)
                                                (abs
                                                  dead
                                                  (type)
                                                  {
                                                    [
                                                      [
                                                        [
                                                          {
                                                            [
                                                              {
                                                                Extended_match
                                                                (con integer)
                                                              }
                                                              ww
                                                            ]
                                                            (all dead (type) Bool)
                                                          }
                                                          (lam
                                                            ipv
                                                            (con integer)
                                                            (abs
                                                              dead
                                                              (type)
                                                              {
                                                                [
                                                                  [
                                                                    [
                                                                      {
                                                                        [
                                                                          Ordering_match
                                                                          [
                                                                            [
                                                                              fOrdInteger_ccompare
                                                                              ipv
                                                                            ]
                                                                            ipv
                                                                          ]
                                                                        ]
                                                                        (all dead (type) Bool)
                                                                      }
                                                                      (abs
                                                                        dead
                                                                        (type)
                                                                        j
                                                                      )
                                                                    ]
                                                                    (abs
                                                                      dead
                                                                      (type)
                                                                      False
                                                                    )
                                                                  ]
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    [
                                                                      [
                                                                        sfOrdUpperBound0_c
                                                                        ww
                                                                      ]
                                                                      ww
                                                                    ]
                                                                  )
                                                                ]
                                                                (all dead (type) dead)
                                                              }
                                                            )
                                                          )
                                                        ]
                                                        (abs
                                                          dead
                                                          (type)
                                                          (error Bool)
                                                        )
                                                      ]
                                                      (abs
                                                        dead
                                                        (type)
                                                        [
                                                          [
                                                            sfOrdUpperBound0_c
                                                            ww
                                                          ]
                                                          ww
                                                        ]
                                                      )
                                                    ]
                                                    (all dead (type) dead)
                                                  }
                                                )
                                              )
                                            ]
                                            (abs dead (type) (error Bool))
                                          ]
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Extended_match
                                                          (con integer)
                                                        }
                                                        ww
                                                      ]
                                                      (all dead (type) Bool)
                                                    }
                                                    (lam
                                                      ipv
                                                      (con integer)
                                                      (abs dead (type) False)
                                                    )
                                                  ]
                                                  (abs dead (type) (error Bool))
                                                ]
                                                (abs dead (type) j)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            )
                          ]
                          (abs
                            dead
                            (type)
                            {
                              [
                                [
                                  [
                                    {
                                      [ { Extended_match (con integer) } ww ]
                                      (all dead (type) Bool)
                                    }
                                    (lam
                                      default_arg0
                                      (con integer)
                                      (abs
                                        dead
                                        (type)
                                        [ [ sfOrdUpperBound0_c ww ] ww ]
                                      )
                                    )
                                  ]
                                  (abs dead (type) j)
                                ]
                                (abs
                                  dead (type) [ [ sfOrdUpperBound0_c ww ] ww ]
                                )
                              ]
                              (all dead (type) dead)
                            }
                          )
                        ]
                        (abs
                          dead
                          (type)
                          {
                            [
                              [
                                [
                                  {
                                    [ { Extended_match (con integer) } ww ]
                                    (all dead (type) Bool)
                                  }
                                  (lam
                                    default_arg0
                                    (con integer)
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [
                                                  {
                                                    Extended_match (con integer)
                                                  }
                                                  ww
                                                ]
                                                (all dead (type) Bool)
                                              }
                                              (lam
                                                ipv
                                                (con integer)
                                                (abs
                                                  dead
                                                  (type)
                                                  {
                                                    [
                                                      [
                                                        [
                                                          {
                                                            [
                                                              {
                                                                Extended_match
                                                                (con integer)
                                                              }
                                                              ww
                                                            ]
                                                            (all dead (type) Bool)
                                                          }
                                                          (lam
                                                            ipv
                                                            (con integer)
                                                            (abs
                                                              dead
                                                              (type)
                                                              {
                                                                [
                                                                  [
                                                                    [
                                                                      {
                                                                        [
                                                                          Ordering_match
                                                                          [
                                                                            [
                                                                              fOrdInteger_ccompare
                                                                              ipv
                                                                            ]
                                                                            ipv
                                                                          ]
                                                                        ]
                                                                        (all dead (type) Bool)
                                                                      }
                                                                      (abs
                                                                        dead
                                                                        (type)
                                                                        j
                                                                      )
                                                                    ]
                                                                    (abs
                                                                      dead
                                                                      (type)
                                                                      False
                                                                    )
                                                                  ]
                                                                  (abs
                                                                    dead
                                                                    (type)
                                                                    [
                                                                      [
                                                                        sfOrdUpperBound0_c
                                                                        ww
                                                                      ]
                                                                      ww
                                                                    ]
                                                                  )
                                                                ]
                                                                (all dead (type) dead)
                                                              }
                                                            )
                                                          )
                                                        ]
                                                        (abs
                                                          dead
                                                          (type)
                                                          (error Bool)
                                                        )
                                                      ]
                                                      (abs
                                                        dead
                                                        (type)
                                                        [
                                                          [
                                                            sfOrdUpperBound0_c
                                                            ww
                                                          ]
                                                          ww
                                                        ]
                                                      )
                                                    ]
                                                    (all dead (type) dead)
                                                  }
                                                )
                                              )
                                            ]
                                            (abs dead (type) (error Bool))
                                          ]
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Extended_match
                                                          (con integer)
                                                        }
                                                        ww
                                                      ]
                                                      (all dead (type) Bool)
                                                    }
                                                    (lam
                                                      ipv
                                                      (con integer)
                                                      (abs dead (type) False)
                                                    )
                                                  ]
                                                  (abs dead (type) (error Bool))
                                                ]
                                                (abs dead (type) j)
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  )
                                ]
                                (abs dead (type) False)
                              ]
                              (abs
                                dead
                                (type)
                                {
                                  [
                                    [
                                      [
                                        {
                                          [
                                            { Extended_match (con integer) } ww
                                          ]
                                          (all dead (type) Bool)
                                        }
                                        (lam
                                          ipv
                                          (con integer)
                                          (abs
                                            dead
                                            (type)
                                            {
                                              [
                                                [
                                                  [
                                                    {
                                                      [
                                                        {
                                                          Extended_match
                                                          (con integer)
                                                        }
                                                        ww
                                                      ]
                                                      (all dead (type) Bool)
                                                    }
                                                    (lam
                                                      ipv
                                                      (con integer)
                                                      (abs
                                                        dead
                                                        (type)
                                                        {
                                                          [
                                                            [
                                                              [
                                                                {
                                                                  [
                                                                    Ordering_match
                                                                    [
                                                                      [
                                                                        fOrdInteger_ccompare
                                                                        ipv
                                                                      ]
                                                                      ipv
                                                                    ]
                                                                  ]
                                                                  (all dead (type) Bool)
                                                                }
                                                                (abs
                                                                  dead (type) j
                                                                )
                                                              ]
                                                              (abs
                                                                dead
                                                                (type)
                                                                False
                                                              )
                                                            ]
                                                            (abs
                                                              dead
                                                              (type)
                                                              [
                                                                [
                                                                  sfOrdUpperBound0_c
                                                                  ww
                                                                ]
                                                                ww
                                                              ]
                                                            )
                                                          ]
                                                          (all dead (type) dead)
                                                        }
                                                      )
                                                    )
                                                  ]
                                                  (abs dead (type) (error Bool))
                                                ]
                                                (abs
                                                  dead
                                                  (type)
                                                  [
                                                    [ sfOrdUpperBound0_c ww ] ww
                                                  ]
                                                )
                                              ]
                                              (all dead (type) dead)
                                            }
                                          )
                                        )
                                      ]
                                      (abs dead (type) (error Bool))
                                    ]
                                    (abs
                                      dead
                                      (type)
                                      {
                                        [
                                          [
                                            [
                                              {
                                                [
                                                  {
                                                    Extended_match (con integer)
                                                  }
                                                  ww
                                                ]
                                                (all dead (type) Bool)
                                              }
                                              (lam
                                                ipv
                                                (con integer)
                                                (abs dead (type) False)
                                              )
                                            ]
                                            (abs dead (type) (error Bool))
                                          ]
                                          (abs dead (type) j)
                                        ]
                                        (all dead (type) dead)
                                      }
                                    )
                                  ]
                                  (all dead (type) dead)
                                }
                              )
                            ]
                            (all dead (type) dead)
                          }
                        )
                      ]
                      (all dead (type) dead)
                    }
                  )
                )
              )
            )
          )
        )
      )
    )
    (termbind
      (strict)
      (vardecl
        wvalidateCrowdfunding
        (fun (con integer) (fun [Extended (con integer)] (fun Bool (fun [UpperBound (con integer)] Bool))))
      )
      (lam
        ww
        (con integer)
        (lam
          ww
          [Extended (con integer)]
          (lam
            ww
            Bool
            (lam
              ww
              [UpperBound (con integer)]
              {
                [
                  [
                    {
                      [
                        Bool_match
                        [
                          [
                            [
                              [
                                [
                                  [ wscontains [ { Finite (con integer) } ww ] ]
                                  True
                                ]
                                [
                                  [
                                    { UpperBound (con integer) }
                                    { PosInf (con integer) }
                                  ]
                                  True
                                ]
                              ]
                              ww
                            ]
                            ww
                          ]
                          ww
                        ]
                      ]
                      (all dead (type) Bool)
                    }
                    (abs dead (type) True)
                  ]
                  (abs
                    dead
                    (type)
                    [
                      [
                        { (builtin chooseUnit) Bool }
                        [
                          (builtin trace)
                          (con string "Cannot close before the deadline")
                        ]
                      ]
                      False
                    ]
                  )
                ]
                (all dead (type) dead)
              }
            )
          )
        )
      )
    )
    (datatypebind
      (datatype (tyvardecl Unit (type))  Unit_match (vardecl Unit Unit))
    )
    (datatypebind
      (datatype
        (tyvardecl CrowdfundingParams (type))

        CrowdfundingParams_match
        (vardecl
          CrowdfundingParams
          (fun (con integer) (fun (con integer) (fun (con bytestring) (fun (con integer) CrowdfundingParams))))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Credential (type))

        Credential_match
        (vardecl PubKeyCredential (fun (con bytestring) Credential))
        (vardecl ScriptCredential (fun (con bytestring) Credential))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl StakingCredential (type))

        StakingCredential_match
        (vardecl StakingHash (fun Credential StakingCredential))
        (vardecl
          StakingPtr
          (fun (con integer) (fun (con integer) (fun (con integer) StakingCredential)))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl DCert (type))

        DCert_match
        (vardecl DCertDelegDeRegKey (fun StakingCredential DCert))
        (vardecl
          DCertDelegDelegate
          (fun StakingCredential (fun (con bytestring) DCert))
        )
        (vardecl DCertDelegRegKey (fun StakingCredential DCert))
        (vardecl DCertGenesis DCert)
        (vardecl DCertMir DCert)
        (vardecl
          DCertPoolRegister (fun (con bytestring) (fun (con bytestring) DCert))
        )
        (vardecl
          DCertPoolRetire (fun (con bytestring) (fun (con integer) DCert))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl TxOutRef (type))

        TxOutRef_match
        (vardecl TxOutRef (fun (con bytestring) (fun (con integer) TxOutRef)))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl ScriptPurpose (type))

        ScriptPurpose_match
        (vardecl Certifying (fun DCert ScriptPurpose))
        (vardecl Minting (fun (con bytestring) ScriptPurpose))
        (vardecl Rewarding (fun StakingCredential ScriptPurpose))
        (vardecl Spending (fun TxOutRef ScriptPurpose))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl LowerBound (fun (type) (type)))
        (tyvardecl a (type))
        LowerBound_match
        (vardecl LowerBound (fun [Extended a] (fun Bool [LowerBound a])))
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Interval (fun (type) (type)))
        (tyvardecl a (type))
        Interval_match
        (vardecl Interval (fun [LowerBound a] (fun [UpperBound a] [Interval a]))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Maybe (fun (type) (type)))
        (tyvardecl a (type))
        Maybe_match
        (vardecl Just (fun a [Maybe a])) (vardecl Nothing [Maybe a])
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Address (type))

        Address_match
        (vardecl
          Address (fun Credential (fun [Maybe StakingCredential] Address))
        )
      )
    )
    (datatypebind
      (datatype
        (tyvardecl Tuple2 (fun (type) (fun (type) (type))))
        (tyvardecl a (type)) (tyvardecl b (type))
        Tuple2_match
        (vardecl Tuple2 (fun a (fun b [[Tuple2 a] b])))
      )
    )
    (let
      (rec)
      (datatypebind
        (datatype
          (tyvardecl List (fun (type) (type)))
          (tyvardecl a (type))
          Nil_match
          (vardecl Nil [List a]) (vardecl Cons (fun a (fun [List a] [List a])))
        )
      )
      (let
        (nonrec)
        (datatypebind
          (datatype
            (tyvardecl TxOut (type))

            TxOut_match
            (vardecl
              TxOut
              (fun Address (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [Maybe (con bytestring)] TxOut)))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl TxInInfo (type))

            TxInInfo_match
            (vardecl TxInInfo (fun TxOutRef (fun TxOut TxInInfo)))
          )
        )
        (datatypebind
          (datatype
            (tyvardecl TxInfo (type))

            TxInfo_match
            (vardecl
              TxInfo
              (fun [List TxInInfo] (fun [List TxOut] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]] (fun [List DCert] (fun [List [[Tuple2 StakingCredential] (con integer)]] (fun [Interval (con integer)] (fun [List (con bytestring)] (fun [List [[Tuple2 (con bytestring)] (con data)]] (fun (con bytestring) TxInfo))))))))))
            )
          )
        )
        (datatypebind
          (datatype
            (tyvardecl ScriptContext (type))

            ScriptContext_match
            (vardecl
              ScriptContext (fun TxInfo (fun ScriptPurpose ScriptContext))
            )
          )
        )
        (termbind
          (strict)
          (vardecl
            validateCrowdfunding
            (fun CrowdfundingParams (fun Unit (fun Unit (fun ScriptContext Bool))))
          )
          (lam
            w
            CrowdfundingParams
            (lam
              w
              Unit
              (lam
                w
                Unit
                (lam
                  w
                  ScriptContext
                  [
                    { [ CrowdfundingParams_match w ] Bool }
                    (lam
                      ww
                      (con integer)
                      (lam
                        ww
                        (con integer)
                        (lam
                          ww
                          (con bytestring)
                          (lam
                            ww
                            (con integer)
                            [
                              { [ ScriptContext_match w ] Bool }
                              (lam
                                ww
                                TxInfo
                                (lam
                                  ww
                                  ScriptPurpose
                                  [
                                    { [ TxInfo_match ww ] Bool }
                                    (lam
                                      ww
                                      [List TxInInfo]
                                      (lam
                                        ww
                                        [List TxOut]
                                        (lam
                                          ww
                                          [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                          (lam
                                            ww
                                            [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] [[(lam k (type) (lam v (type) [List [[Tuple2 k] v]])) (con bytestring)] (con integer)]]
                                            (lam
                                              ww
                                              [List DCert]
                                              (lam
                                                ww
                                                [List [[Tuple2 StakingCredential] (con integer)]]
                                                (lam
                                                  ww
                                                  [Interval (con integer)]
                                                  (lam
                                                    ww
                                                    [List (con bytestring)]
                                                    (lam
                                                      ww
                                                      [List [[Tuple2 (con bytestring)] (con data)]]
                                                      (lam
                                                        ww
                                                        (con bytestring)
                                                        [
                                                          {
                                                            [
                                                              {
                                                                Interval_match
                                                                (con integer)
                                                              }
                                                              ww
                                                            ]
                                                            Bool
                                                          }
                                                          (lam
                                                            ww
                                                            [LowerBound (con integer)]
                                                            (lam
                                                              ww
                                                              [UpperBound (con integer)]
                                                              [
                                                                {
                                                                  [
                                                                    {
                                                                      LowerBound_match
                                                                      (con integer)
                                                                    }
                                                                    ww
                                                                  ]
                                                                  Bool
                                                                }
                                                                (lam
                                                                  ww
                                                                  [Extended (con integer)]
                                                                  (lam
                                                                    ww
                                                                    Bool
                                                                    [
                                                                      [
                                                                        [
                                                                          [
                                                                            wvalidateCrowdfunding
                                                                            ww
                                                                          ]
                                                                          ww
                                                                        ]
                                                                        ww
                                                                      ]
                                                                      ww
                                                                    ]
                                                                  )
                                                                )
                                                              ]
                                                            )
                                                          )
                                                        ]
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ]
                                )
                              )
                            ]
                          )
                        )
                      )
                    )
                  ]
                )
              )
            )
          )
        )
        validateCrowdfunding
      )
    )
  )
)