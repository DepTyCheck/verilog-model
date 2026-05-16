-- Seed: 8814176463386394347,18424117564733761959



entity dcq is
  port (pm : inout severity_level; pzmeyfwsr : buffer time; xpowcysjyw : out real; npipuxr : linkage real);
end dcq;



architecture svhbpjzip of dcq is
  
begin
  
end svhbpjzip;

library ieee;
use ieee.std_logic_1164.all;

entity jhvhb is
  port (erxtm : buffer time; tribjtkih : out integer; gkengdkyr : out severity_level; go : linkage std_logic);
end jhvhb;



architecture cli of jhvhb is
  signal qxqgtbcqzh : real;
begin
  ffo : entity work.dcq
    port map (pm => gkengdkyr, pzmeyfwsr => erxtm, xpowcysjyw => qxqgtbcqzh, npipuxr => qxqgtbcqzh);
end cli;



entity ctuqqkos is
  port (yo : out time; loapyr : linkage time);
end ctuqqkos;



architecture ti of ctuqqkos is
  
begin
  
end ti;

library ieee;
use ieee.std_logic_1164.all;

entity yxvt is
  port (dbgdxcet : linkage boolean; vbfftj : buffer std_logic);
end yxvt;



architecture xq of yxvt is
  signal ewj : time;
  signal xjmbyaeg : time;
  signal tzitww : time;
  signal wurmrshyai : real;
  signal agtqkhnu : real;
  signal kecjyk : time;
  signal oft : severity_level;
begin
  f : entity work.dcq
    port map (pm => oft, pzmeyfwsr => kecjyk, xpowcysjyw => agtqkhnu, npipuxr => wurmrshyai);
  rsm : entity work.ctuqqkos
    port map (yo => tzitww, loapyr => xjmbyaeg);
  xihv : entity work.ctuqqkos
    port map (yo => xjmbyaeg, loapyr => ewj);
end xq;



-- Seed after: 388349973010373395,18424117564733761959
