-- Seed: 17594028710240058931,1420300365443511127



entity ielsqofxe is
  port (gwtebimdt : buffer real; filjegotk : out severity_level; bxqnjv : inout real; u : out real);
end ielsqofxe;



architecture lilzzxmddx of ielsqofxe is
  
begin
  
end lilzzxmddx;



entity ngorli is
  port (kjrffg : inout time; rzcg : out integer; bflcvfg : in boolean);
end ngorli;



architecture svcwyte of ngorli is
  signal tbo : real;
  signal nqd : real;
  signal ddppk : severity_level;
  signal knrdjmc : real;
  signal grgcemsq : real;
  signal l : real;
  signal kophirqajy : severity_level;
  signal dsphjsp : real;
  signal wizekpjp : real;
  signal nwcqynzwth : real;
  signal niarzyfv : severity_level;
  signal eut : real;
begin
  uncr : entity work.ielsqofxe
    port map (gwtebimdt => eut, filjegotk => niarzyfv, bxqnjv => nwcqynzwth, u => wizekpjp);
  ebrcj : entity work.ielsqofxe
    port map (gwtebimdt => dsphjsp, filjegotk => kophirqajy, bxqnjv => l, u => grgcemsq);
  xmmxszs : entity work.ielsqofxe
    port map (gwtebimdt => knrdjmc, filjegotk => ddppk, bxqnjv => nqd, u => tbo);
end svcwyte;

library ieee;
use ieee.std_logic_1164.all;

entity owudti is
  port (mgfwrth : linkage integer; zzenod : linkage std_logic; pcx : out time; lot : in bit);
end owudti;



architecture ks of owudti is
  signal paeqnacqw : boolean;
  signal dt : integer;
  signal fopwjju : real;
  signal ovrdorp : real;
  signal cnsq : severity_level;
  signal tfqugclf : real;
  signal rm : real;
  signal qiocucweo : real;
  signal rrdlgix : severity_level;
  signal wedtcgdi : real;
  signal agugur : real;
  signal pqmuerjfv : real;
  signal vmznsirjt : severity_level;
  signal p : real;
begin
  hu : entity work.ielsqofxe
    port map (gwtebimdt => p, filjegotk => vmznsirjt, bxqnjv => pqmuerjfv, u => agugur);
  ao : entity work.ielsqofxe
    port map (gwtebimdt => wedtcgdi, filjegotk => rrdlgix, bxqnjv => qiocucweo, u => rm);
  euywcysjf : entity work.ielsqofxe
    port map (gwtebimdt => tfqugclf, filjegotk => cnsq, bxqnjv => ovrdorp, u => fopwjju);
  bwnvaek : entity work.ngorli
    port map (kjrffg => pcx, rzcg => dt, bflcvfg => paeqnacqw);
end ks;



-- Seed after: 17191227882908708656,1420300365443511127
