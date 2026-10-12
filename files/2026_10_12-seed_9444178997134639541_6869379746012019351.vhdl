-- Seed: 9444178997134639541,6869379746012019351

entity jpdmgfrdzb is
  port (kwp : out time; keix : inout real; temonx : buffer time; srokhtz : buffer time);
end jpdmgfrdzb;

architecture uf of jpdmgfrdzb is
  
begin
  -- Single-driven assignments
  keix <= 2432.0_3;
  kwp <= 2#011.0_1_1_0_1# us;
  temonx <= kwp;
  srokhtz <= 1 hr;
end uf;

entity qfeknndfj is
  port (qtxhe : linkage integer; gohuzms : buffer character; drx : out time);
end qfeknndfj;

architecture tjzgq of qfeknndfj is
  signal yuumwxgza : time;
  signal xvdyyqh : real;
  signal grezhvt : time;
  signal j : time;
  signal lxfsk : time;
  signal mlgeagyti : real;
  signal rogszfe : time;
begin
  hdmygcknr : entity work.jpdmgfrdzb
    port map (kwp => rogszfe, keix => mlgeagyti, temonx => lxfsk, srokhtz => j);
  jcrpckpse : entity work.jpdmgfrdzb
    port map (kwp => grezhvt, keix => xvdyyqh, temonx => drx, srokhtz => yuumwxgza);
  
  -- Single-driven assignments
  gohuzms <= 'q';
end tjzgq;

library ieee;
use ieee.std_logic_1164.all;

entity enpmgjih is
  port (dcjbvgauek : out real; zhulkp : out std_logic; bsr : in string(5 to 4));
end enpmgjih;

architecture zgbogzpig of enpmgjih is
  signal bssrssxd : time;
  signal lwh : time;
  signal gnbjtrensa : time;
begin
  uvmdcv : entity work.jpdmgfrdzb
    port map (kwp => gnbjtrensa, keix => dcjbvgauek, temonx => lwh, srokhtz => bssrssxd);
end zgbogzpig;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (qqul : buffer std_logic; slsjbroccr : out bit);
end k;

architecture yxesqgtzt of k is
  signal elokkik : time;
  signal ppmso : time;
  signal igezchphzl : real;
  signal xwwvp : time;
begin
  qgmktpagsv : entity work.jpdmgfrdzb
    port map (kwp => xwwvp, keix => igezchphzl, temonx => ppmso, srokhtz => elokkik);
  
  -- Multi-driven assignments
  qqul <= '0';
  qqul <= qqul;
  qqul <= qqul;
end yxesqgtzt;



-- Seed after: 5767984171752170443,6869379746012019351
