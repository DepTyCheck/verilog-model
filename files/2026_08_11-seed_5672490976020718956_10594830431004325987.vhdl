-- Seed: 5672490976020718956,10594830431004325987

entity zttzpcig is
  port (zezveaxwsr : in real; epbib : linkage time);
end zttzpcig;

architecture xtq of zttzpcig is
  
begin
  
end xtq;

library ieee;
use ieee.std_logic_1164.all;

entity ern is
  port (vnx : buffer integer; sfjqe : in std_logic_vector(1 downto 4); fqwhhlvrv : out real; pmphwrq : out time);
end ern;

architecture lkqhlqmcd of ern is
  signal jshuhkcf : time;
  signal jzn : real;
begin
  lp : entity work.zttzpcig
    port map (zezveaxwsr => jzn, epbib => jshuhkcf);
  djvne : entity work.zttzpcig
    port map (zezveaxwsr => fqwhhlvrv, epbib => pmphwrq);
  
  -- Single-driven assignments
  fqwhhlvrv <= fqwhhlvrv;
  jzn <= jzn;
  vnx <= vnx;
end lkqhlqmcd;

entity bftexc is
  port (yhwpeolbg : linkage integer; harfe : linkage integer);
end bftexc;

library ieee;
use ieee.std_logic_1164.all;

architecture pfkgnisfz of bftexc is
  signal vkoaomcet : time;
  signal jxwxfbgzd : time;
  signal klqmazptz : real;
  signal qtcm : std_logic_vector(1 downto 4);
  signal m : integer;
begin
  pbmk : entity work.ern
    port map (vnx => m, sfjqe => qtcm, fqwhhlvrv => klqmazptz, pmphwrq => jxwxfbgzd);
  mjfgnmp : entity work.zttzpcig
    port map (zezveaxwsr => klqmazptz, epbib => vkoaomcet);
  
  -- Multi-driven assignments
  qtcm <= "";
end pfkgnisfz;

entity cx is
  port (iwxa : out integer);
end cx;

architecture mxop of cx is
  signal lhlzyqsw : time;
  signal cbefz : real;
  signal ztjpeauazs : integer;
  signal bv : integer;
begin
  iupvjydjvl : entity work.bftexc
    port map (yhwpeolbg => bv, harfe => ztjpeauazs);
  jom : entity work.zttzpcig
    port map (zezveaxwsr => cbefz, epbib => lhlzyqsw);
end mxop;



-- Seed after: 3733174707284956539,10594830431004325987
