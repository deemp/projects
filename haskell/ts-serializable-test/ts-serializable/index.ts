
import 'reflect-metadata'
import {
  jsonObject,
  jsonProperty,
  jsonName,
  Serializable,
  SnakeCaseNamingStrategy,
  KebabCaseNamingStrategy,
  PascalCaseNamingStrategy
} from "ts-serializable";
import { writeFileSync } from 'fs';
import { join } from 'path';


@jsonObject({ namingStrategy: new SnakeCaseNamingStrategy() })
export class EZJC extends Serializable {
  @jsonName("y7ASSkU")
  @jsonProperty(Number)
  public mT5yGP: number = -6;

  @jsonName("v1Ir24Qw")
  @jsonProperty(Number, void 25)
  public tingnU5dWhc?: number = void 25;

  @jsonName("xl1q")
  @jsonProperty(Date, Number)
  private z4xze: Date | number = new Date("1973-12-25 08:49:37 UTC");

  private h9ukJedxX6(): string { return ""; };

  public dpDKN7rxo(): number { return 0; };

  private nfX(): string { return ""; };

}

@jsonObject({ namingStrategy: new PascalCaseNamingStrategy() })
export class Wcqs extends Serializable {
  @jsonName("uVOBV")
  @jsonProperty([String], [Date])
  private c4hYXHSYd: [string] | [Date] = [new Date("2082-05-21 17:21:43 UTC")];

  @jsonName("rdMCWiFI")
  @jsonProperty([Date], Date)
  private wfo3KxAikY: [Date] | Date = new Date("2019-01-15 05:29:15 UTC");

  private eRESP0Wy8(): string { return ""; };

  public pHP8UdqFMvE(): number { return 0; };

}

@jsonObject({ namingStrategy: new SnakeCaseNamingStrategy() })
export class FfrE extends Serializable {
  @jsonName("lkpq")
  @jsonProperty([EZJC])
  private tJ6LyErba1: [EZJC] = [new EZJC()];

  @jsonName("lyNS")
  @jsonProperty(Date, EZJC, void 22)
  public l2pzG0?: Date | EZJC = void 22;

  @jsonName("uxRu59ffnWu")
  @jsonProperty(EZJC, [Date])
  public sAmFrUSwN: EZJC | [Date] = [new Date("2050-08-02 06:17:37 UTC")];

  @jsonName("kVGt4kehg")
  @jsonProperty(void 26, [EZJC])
  public b2T1ZEW?: [EZJC] = void 26;

  private lWuk(): string { return ""; };

  public k92oUbxCO2(): number { return 0; };

  public pry(): string { return ""; };

}

@jsonObject({ namingStrategy: new PascalCaseNamingStrategy() })
export class RLSvDWyE6 extends Serializable {
  
  @jsonProperty([FfrE])
  private mUwSvuKIl: [FfrE] = [new FfrE()];

  @jsonName("pk9p")
  @jsonProperty(FfrE, [String])
  public q7aRrH0: FfrE | [string] = ["2Gzac8wAaS"];

  @jsonName("s7Y")
  @jsonProperty(String, FfrE)
  public t9hBXc: string | FfrE = "QQCVCV91A";

  public tk1mCW6KbXd(): string { return ""; };

  public wBpeE(): number { return 0; };

  public b16qyCKTWJM(): string { return ""; };

  public xLMf(): number { return 0; };

  public thWD(): number { return 0; };

}

@jsonObject({ namingStrategy: new SnakeCaseNamingStrategy() })
export class KpSIO3d9x extends Serializable {
  @jsonName("ugC")
  @jsonProperty([FfrE], [RLSvDWyE6])
  private iTqASTYqqO: [FfrE] | [RLSvDWyE6] = [new FfrE()];

  @jsonName("z4H")
  @jsonProperty(FfrE)
  public h16D4p1SGq: FfrE = new FfrE();

  @jsonName("xkeAmAesCy")
  @jsonProperty([Wcqs], [RLSvDWyE6])
  private em0YTabhsSn: [Wcqs] | [RLSvDWyE6] = [new Wcqs()];

  private thp(): number { return 0; };

  public kk4Xa(): number { return 0; };

  private dK1zN7f(): string { return ""; };

  private fxOgSLgQ(): string { return ""; };

}

@jsonObject({ namingStrategy: new SnakeCaseNamingStrategy() })
export class J5nirTU2 extends Serializable {
  @jsonName("uPrYyPaG")
  @jsonProperty([KpSIO3d9x], Date)
  public kkMHaaL: [KpSIO3d9x] | Date = [new KpSIO3d9x()];

  @jsonName("oF5")
  @jsonProperty(Wcqs)
  public xaWlrOw4VO: Wcqs = new Wcqs();

  @jsonName("ymz7k8yf738")
  @jsonProperty(RLSvDWyE6, [Wcqs])
  public c7ExV6X: RLSvDWyE6 | [Wcqs] = new RLSvDWyE6();

  public xMJq0Htp5q(): number { return 0; };

  private sBXi1k6yXzM(): number { return 0; };

}

@jsonObject({ namingStrategy: new KebabCaseNamingStrategy() })
export class TlnOy2FqoI extends Serializable {
  @jsonName("tQQDxORpHZ")
  @jsonProperty(EZJC, KpSIO3d9x, [KpSIO3d9x])
  public zWyvY0PwH5P: EZJC | KpSIO3d9x | [KpSIO3d9x] = new KpSIO3d9x();

  @jsonName("luc")
  @jsonProperty([EZJC], [Date], EZJC)
  private ab4Y: [EZJC] | [Date] | EZJC = [new Date("2087-07-23 07:41:56 UTC")];

  private sa699X(): string { return ""; };

  private r6iOss(): string { return ""; };

  private vOv5yk5(): number { return 0; };

  public dwYSY(): number { return 0; };

  private vaAzooRpM(): string { return ""; };

}

@jsonObject({ namingStrategy: new SnakeCaseNamingStrategy() })
export class PELMAU extends Serializable {
  
  @jsonProperty(Wcqs, J5nirTU2)
  public szxw: Wcqs | J5nirTU2 = new Wcqs();

  @jsonName("c19k")
  @jsonProperty(Number, J5nirTU2)
  private vxmeJQH: number | J5nirTU2 = new J5nirTU2();

  @jsonName("zKXd")
  @jsonProperty([J5nirTU2], [FfrE])
  public zTtZItax: [J5nirTU2] | [FfrE] = [new J5nirTU2()];

  @jsonName("cUQaQ5tePn")
  @jsonProperty(FfrE)
  private l8VL75: FfrE = new FfrE();

  private dXCw0(): string { return ""; };

  public tSzl(): number { return 0; };

  private lUwvcv16MGn(): number { return 0; };

  public eK5w9p9q(): number { return 0; };

}

@jsonObject({ namingStrategy: new KebabCaseNamingStrategy() })
export class SrkTiM3u0B9 extends Serializable {
  
  @jsonProperty([KpSIO3d9x], [J5nirTU2], [Date])
  public ayo: [KpSIO3d9x] | [J5nirTU2] | [Date] = [new KpSIO3d9x()];

  @jsonName("uWKUATAX")
  @jsonProperty([Date])
  private s99u70acw: [Date] = [new Date("2083-07-12 12:21:34 UTC")];

  @jsonName("k7BB")
  @jsonProperty(void -23, J5nirTU2)
  public p8q71hU3O: J5nirTU2 = new J5nirTU2();

  @jsonName("r7xQEoL")
  @jsonProperty(void 49, [Wcqs], Wcqs)
  public jGRwWQClnv?: [Wcqs] | Wcqs = void 49;

  public mCiiyVXGg3H(): string { return ""; };

  private jEwGN4ZIa00(): number { return 0; };

  public mVD(): number { return 0; };

  private jD8x8M(): string { return ""; };

  public dTdKwkIr9X(): string { return ""; };

}

@jsonObject({ namingStrategy: new KebabCaseNamingStrategy() })
export class E0OdVWzK extends Serializable {
  @jsonName("efaRvn")
  @jsonProperty(EZJC, [SrkTiM3u0B9], [EZJC])
  public fJsUt7: EZJC | [SrkTiM3u0B9] | [EZJC] = [new EZJC()];

  @jsonName("aYFMEonax6r")
  @jsonProperty([PELMAU], EZJC)
  private j0z: [PELMAU] | EZJC = [new PELMAU()];

  @jsonName("eiUCaDYt")
  @jsonProperty([SrkTiM3u0B9])
  private fUZsIKxfNO: [SrkTiM3u0B9] = [new SrkTiM3u0B9()];

  @jsonName("cX5XG")
  @jsonProperty(Date)
  public wU3: Date = new Date("2073-09-26 15:50:03 UTC");

  @jsonName("zGhJYfd76")
  @jsonProperty(Date)
  public k2Yew: Date = new Date("2028-06-30 08:28:04 UTC");

  private yufeWOKCR(): string { return ""; };

  public iFe(): string { return ""; };

}

@jsonObject({ namingStrategy: new KebabCaseNamingStrategy() })
export class Qb8vJHRtA extends Serializable {
  @jsonName("lvi")
  @jsonProperty(EZJC)
  public hMnxhiaozL: EZJC = new EZJC();

  @jsonName("kgq")
  @jsonProperty(Wcqs)
  public bVb: Wcqs = new Wcqs();

  
  @jsonProperty(FfrE)
  public lXo: FfrE = new FfrE();

  @jsonName("s4m")
  @jsonProperty(RLSvDWyE6)
  public svNy5CCoo: RLSvDWyE6 = new RLSvDWyE6();

  @jsonName("x2H")
  @jsonProperty(KpSIO3d9x)
  public iyirnk3zZ: KpSIO3d9x = new KpSIO3d9x();

  @jsonName("aO2Tyr")
  @jsonProperty(J5nirTU2)
  public muyL2N: J5nirTU2 = new J5nirTU2();

  
  @jsonProperty(TlnOy2FqoI)
  private t9FaYN7: TlnOy2FqoI = new TlnOy2FqoI();

  
  @jsonProperty(PELMAU)
  private jp5MYQ0: PELMAU = new PELMAU();

  @jsonName("cFaD0dzrQUd")
  @jsonProperty(SrkTiM3u0B9)
  public u9GsqvhY: SrkTiM3u0B9 = new SrkTiM3u0B9();

  @jsonName("cigpTlLRzSH")
  @jsonProperty(E0OdVWzK)
  private kwDC297: E0OdVWzK = new E0OdVWzK();

  public nHPDOPe9pV(): string { return ""; };

  public yJNKY(): string { return ""; };

  public ytDavI(): string { return ""; };

}
const toJSONMessage = "\n\nto JSON\n\n"
console.log(toJSONMessage)

const topToJSON = new Qb8vJHRtA().toJSON()
// console.log(topToJSON)

const fname = "result.txt"

function syncWriteFile(filename: string = fname, data: any) {
  writeFileSync(join(__dirname, filename), JSON.stringify(data, null, 2), {
    flag: 'w',
  });
}

syncWriteFile(fname, toJSONMessage)
syncWriteFile(fname, topToJSON)

const fromJSONMessage = "\n\nfrom JSON\n\n"
console.log(fromJSONMessage)

const topFromJSON = Qb8vJHRtA.fromJSON(topToJSON)

// console.log(topFromJSON)

syncWriteFile(fname, fromJSONMessage)
syncWriteFile(fname, topFromJSON)
      