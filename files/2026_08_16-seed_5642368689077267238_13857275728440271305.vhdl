-- Seed: 5642368689077267238,13857275728440271305

entity isixialdv is
  port (aiycnuw : linkage real; cczbzb : in real);
end isixialdv;

architecture mmtad of isixialdv is
  
begin
  
end mmtad;

entity qaxjxg is
  port (aunkg : inout time; lxcfpv : linkage boolean);
end qaxjxg;

architecture ehpy of qaxjxg is
  signal kgfgjn : real;
  signal irwdktwut : real;
  signal yhptx : real;
  signal lwr : real;
begin
  zfuyhbsqtv : entity work.isixialdv
    port map (aiycnuw => lwr, cczbzb => yhptx);
  mv : entity work.isixialdv
    port map (aiycnuw => irwdktwut, cczbzb => kgfgjn);
  lyvfdtahbf : entity work.isixialdv
    port map (aiycnuw => yhptx, cczbzb => yhptx);
  bn : entity work.isixialdv
    port map (aiycnuw => kgfgjn, cczbzb => lwr);
end ehpy;

entity yhtdczsgpw is
  port (qwbctj : in integer; vqhdrfmafu : inout character; rdffbsnpnb : linkage time_vector(4 to 2));
end yhtdczsgpw;

architecture h of yhtdczsgpw is
  signal urblyz : boolean;
  signal edmieov : time;
  signal wy : real;
  signal pkrjkpmxph : real;
begin
  hwl : entity work.isixialdv
    port map (aiycnuw => pkrjkpmxph, cczbzb => wy);
  twjmyd : entity work.qaxjxg
    port map (aunkg => edmieov, lxcfpv => urblyz);
  
  -- Single-driven assignments
  wy <= pkrjkpmxph;
  vqhdrfmafu <= 'p';
end h;

library ieee;
use ieee.std_logic_1164.all;

entity wcydwswfg is
  port (eydkkebh : linkage std_logic_vector(1 to 4));
end wcydwswfg;

architecture zmanohstzh of wcydwswfg is
  signal yxsepdkf : real;
  signal whubbrmre : real;
  signal fklhwki : time_vector(4 to 2);
  signal jgs : character;
  signal zelioore : integer;
begin
  dpahzvkmi : entity work.yhtdczsgpw
    port map (qwbctj => zelioore, vqhdrfmafu => jgs, rdffbsnpnb => fklhwki);
  tbftvewkp : entity work.isixialdv
    port map (aiycnuw => whubbrmre, cczbzb => yxsepdkf);
  
  -- Single-driven assignments
  zelioore <= zelioore;
  yxsepdkf <= 0_3_3_3.041;
end zmanohstzh;



-- Seed after: 12684718252650336011,13857275728440271305
