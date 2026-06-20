-- Seed: 11225492736935656980,3924983747739634027

library ieee;
use ieee.std_logic_1164.all;

entity ekxzjjrlp is
  port (yimy : in std_logic_vector(0 to 3); hcfyjnqzxz : in integer);
end ekxzjjrlp;

architecture ymnv of ekxzjjrlp is
  
begin
  
end ymnv;

library ieee;
use ieee.std_logic_1164.all;

entity hch is
  port (irxjk : buffer std_logic; qauhshtj : in std_logic; lskcvh : inout std_logic_vector(0 to 2));
end hch;

architecture zhnpik of hch is
  
begin
  -- Multi-driven assignments
  lskcvh <= "ZHU";
end zhnpik;

library ieee;
use ieee.std_logic_1164.all;

entity pysrdupl is
  port (qmittwdhg : inout severity_level; fz : buffer std_logic_vector(2 to 4); uqmjfr : out time_vector(1 downto 3));
end pysrdupl;

library ieee;
use ieee.std_logic_1164.all;

architecture yhedwpor of pysrdupl is
  signal dklfbga : integer;
  signal rzv : std_logic_vector(0 to 3);
begin
  mkgxbunxv : entity work.ekxzjjrlp
    port map (yimy => rzv, hcfyjnqzxz => dklfbga);
  
  -- Single-driven assignments
  uqmjfr <= (others => 0 ns);
  qmittwdhg <= WARNING;
  dklfbga <= 8#1#;
end yhedwpor;

library ieee;
use ieee.std_logic_1164.all;

entity alxpimhn is
  port (y : inout std_logic; mncpdwskrc : in time);
end alxpimhn;

library ieee;
use ieee.std_logic_1164.all;

architecture szph of alxpimhn is
  signal wtowj : std_logic_vector(0 to 3);
  signal kqdjzvnyai : integer;
  signal rfiq : std_logic_vector(0 to 3);
  signal w : time_vector(1 downto 3);
  signal yafirwd : std_logic_vector(2 to 4);
  signal ljbf : severity_level;
  signal qz : integer;
  signal nhdrd : std_logic_vector(0 to 3);
begin
  wnmidxdlkk : entity work.ekxzjjrlp
    port map (yimy => nhdrd, hcfyjnqzxz => qz);
  sykjnhj : entity work.pysrdupl
    port map (qmittwdhg => ljbf, fz => yafirwd, uqmjfr => w);
  ls : entity work.ekxzjjrlp
    port map (yimy => rfiq, hcfyjnqzxz => kqdjzvnyai);
  x : entity work.ekxzjjrlp
    port map (yimy => wtowj, hcfyjnqzxz => kqdjzvnyai);
  
  -- Single-driven assignments
  qz <= 16#51#;
  kqdjzvnyai <= 0_1_3;
  
  -- Multi-driven assignments
  wtowj <= "WZU1";
  y <= '0';
  rfiq <= "UX0-";
end szph;



-- Seed after: 3675075396006846317,3924983747739634027
