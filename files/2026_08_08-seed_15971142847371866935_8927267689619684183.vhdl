-- Seed: 15971142847371866935,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port (ww : inout time; j : out real; ijb : out std_logic; hj : out boolean_vector(2 to 1));
end z;

architecture htqxml of z is
  
begin
  -- Single-driven assignments
  hj <= (others => TRUE);
  
  -- Multi-driven assignments
  ijb <= '0';
end htqxml;

library ieee;
use ieee.std_logic_1164.all;

entity xpugzitwae is
  port (nrw : out real; patpiuwh : in std_logic; r : out real; pgh : inout real);
end xpugzitwae;

library ieee;
use ieee.std_logic_1164.all;

architecture mmrptzd of xpugzitwae is
  signal gqlsjwefu : boolean_vector(2 to 1);
  signal flnkwfll : std_logic;
  signal kda : time;
  signal wdhboiusz : boolean_vector(2 to 1);
  signal xft : std_logic;
  signal w : time;
  signal xukxqonnl : boolean_vector(2 to 1);
  signal wt : std_logic;
  signal fwmi : time;
begin
  b : entity work.z
    port map (ww => fwmi, j => pgh, ijb => wt, hj => xukxqonnl);
  qo : entity work.z
    port map (ww => w, j => r, ijb => xft, hj => wdhboiusz);
  wtxe : entity work.z
    port map (ww => kda, j => nrw, ijb => flnkwfll, hj => gqlsjwefu);
  
  -- Multi-driven assignments
  xft <= patpiuwh;
  flnkwfll <= patpiuwh;
  flnkwfll <= xft;
end mmrptzd;

library ieee;
use ieee.std_logic_1164.all;

entity sdsxcovugq is
  port (geftbeh : inout std_logic; gty : in std_logic_vector(3 downto 3));
end sdsxcovugq;

library ieee;
use ieee.std_logic_1164.all;

architecture q of sdsxcovugq is
  signal shgwhtqbld : real;
  signal scjdm : real;
  signal wdlhatgivn : std_logic;
  signal yepqhpyspe : real;
begin
  aoy : entity work.xpugzitwae
    port map (nrw => yepqhpyspe, patpiuwh => wdlhatgivn, r => scjdm, pgh => shgwhtqbld);
end q;

entity vzdvdj is
  port (vkjh : buffer integer; mqhw : linkage integer; uolykzdb : linkage integer; af : buffer real);
end vzdvdj;

library ieee;
use ieee.std_logic_1164.all;

architecture dadae of vzdvdj is
  signal sdoibubmz : std_logic_vector(3 downto 3);
  signal jaiudapknh : std_logic;
  signal rdupxa : boolean_vector(2 to 1);
  signal yjnbhql : std_logic;
  signal xfmiuun : time;
  signal esdfgnj : boolean_vector(2 to 1);
  signal hwjbwm : real;
  signal jqddp : time;
  signal qyspmes : boolean_vector(2 to 1);
  signal rqoktpsf : std_logic;
  signal eq : real;
  signal xzxrkpsbzh : time;
begin
  ozpzzzsjn : entity work.z
    port map (ww => xzxrkpsbzh, j => eq, ijb => rqoktpsf, hj => qyspmes);
  igz : entity work.z
    port map (ww => jqddp, j => hwjbwm, ijb => rqoktpsf, hj => esdfgnj);
  xeb : entity work.z
    port map (ww => xfmiuun, j => af, ijb => yjnbhql, hj => rdupxa);
  xd : entity work.sdsxcovugq
    port map (geftbeh => jaiudapknh, gty => sdoibubmz);
  
  -- Single-driven assignments
  vkjh <= vkjh;
  
  -- Multi-driven assignments
  rqoktpsf <= rqoktpsf;
  rqoktpsf <= 'H';
  rqoktpsf <= 'L';
end dadae;



-- Seed after: 278526923181146212,8927267689619684183
