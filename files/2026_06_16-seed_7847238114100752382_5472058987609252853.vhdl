-- Seed: 7847238114100752382,5472058987609252853

library ieee;
use ieee.std_logic_1164.all;

entity qgeu is
  port (lr : linkage integer; nqsvd : linkage std_logic_vector(1 to 1); ulmfrvyiid : out time);
end qgeu;

architecture ftqznz of qgeu is
  
begin
  
end ftqznz;

entity llao is
  port (y : linkage integer; lq : in time);
end llao;

library ieee;
use ieee.std_logic_1164.all;

architecture vuhweo of llao is
  signal lygpcnihxs : time;
  signal ncxuumiee : integer;
  signal v : time;
  signal yj : integer;
  signal i : time;
  signal vy : std_logic_vector(1 to 1);
  signal iwc : integer;
  signal ojpooiilj : time;
  signal urjwotsmu : std_logic_vector(1 to 1);
begin
  bnbxrkl : entity work.qgeu
    port map (lr => y, nqsvd => urjwotsmu, ulmfrvyiid => ojpooiilj);
  dajhlxfwmj : entity work.qgeu
    port map (lr => iwc, nqsvd => vy, ulmfrvyiid => i);
  itskrrnnjl : entity work.qgeu
    port map (lr => yj, nqsvd => urjwotsmu, ulmfrvyiid => v);
  oksyl : entity work.qgeu
    port map (lr => ncxuumiee, nqsvd => urjwotsmu, ulmfrvyiid => lygpcnihxs);
  
  -- Multi-driven assignments
  urjwotsmu <= "U";
  urjwotsmu <= (others => 'U');
  urjwotsmu <= "L";
end vuhweo;

entity k is
  port (nwnd : out integer);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture ljuomv of k is
  signal numbdpt : time;
  signal y : std_logic_vector(1 to 1);
  signal avyikavch : integer;
  signal nkxytup : integer;
  signal pqcqxd : time;
  signal jvhjx : std_logic_vector(1 to 1);
  signal hpmg : integer;
begin
  om : entity work.qgeu
    port map (lr => hpmg, nqsvd => jvhjx, ulmfrvyiid => pqcqxd);
  wknurxrrrd : entity work.llao
    port map (y => nkxytup, lq => pqcqxd);
  iy : entity work.qgeu
    port map (lr => avyikavch, nqsvd => y, ulmfrvyiid => numbdpt);
  
  -- Single-driven assignments
  nwnd <= 16#A9#;
  
  -- Multi-driven assignments
  jvhjx <= "W";
end ljuomv;



-- Seed after: 3911687229798799897,5472058987609252853
