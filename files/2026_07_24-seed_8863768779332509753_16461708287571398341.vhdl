-- Seed: 8863768779332509753,16461708287571398341

entity fqtufzx is
  port (tscwdjii : in integer);
end fqtufzx;

architecture wcojhbyr of fqtufzx is
  
begin
  
end wcojhbyr;

library ieee;
use ieee.std_logic_1164.all;

entity tqd is
  port (ycubpnnwvi : buffer time; ufpb : in std_logic);
end tqd;

architecture vihgpncvt of tqd is
  signal h : integer;
  signal aeqdufepqj : integer;
begin
  kr : entity work.fqtufzx
    port map (tscwdjii => aeqdufepqj);
  rqfblq : entity work.fqtufzx
    port map (tscwdjii => h);
  
  -- Single-driven assignments
  h <= 210;
  ycubpnnwvi <= 16#D2901# fs;
end vihgpncvt;

entity avxujhkx is
  port (ica : buffer time_vector(2 downto 4));
end avxujhkx;

library ieee;
use ieee.std_logic_1164.all;

architecture hdacpzfrnm of avxujhkx is
  signal kspnej : time;
  signal qra : std_logic;
  signal gxx : time;
  signal ngjcybaqcm : integer;
begin
  qe : entity work.fqtufzx
    port map (tscwdjii => ngjcybaqcm);
  ttnkpiy : entity work.fqtufzx
    port map (tscwdjii => ngjcybaqcm);
  bdxc : entity work.tqd
    port map (ycubpnnwvi => gxx, ufpb => qra);
  urc : entity work.tqd
    port map (ycubpnnwvi => kspnej, ufpb => qra);
  
  -- Multi-driven assignments
  qra <= qra;
  qra <= 'L';
  qra <= '0';
  qra <= qra;
end hdacpzfrnm;

library ieee;
use ieee.std_logic_1164.all;

entity irzg is
  port (tb : in std_logic; riq : out bit; okgof : out std_logic);
end irzg;

architecture ygl of irzg is
  signal tbosc : time_vector(2 downto 4);
  signal v : integer;
begin
  cx : entity work.fqtufzx
    port map (tscwdjii => v);
  xunqbt : entity work.avxujhkx
    port map (ica => tbosc);
  lgwzans : entity work.fqtufzx
    port map (tscwdjii => v);
  
  -- Single-driven assignments
  v <= v;
  riq <= riq;
  
  -- Multi-driven assignments
  okgof <= tb;
end ygl;



-- Seed after: 3237719086856395035,16461708287571398341
