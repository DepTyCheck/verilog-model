-- Seed: 15226336751953979151,16188444798499499427

library ieee;
use ieee.std_logic_1164.all;

entity ci is
  port (aaiqd : inout std_logic_vector(4 downto 4); rkstxfyru : linkage time; p : in character);
end ci;

architecture vy of ci is
  
begin
  -- Multi-driven assignments
  aaiqd <= "W";
end vy;

library ieee;
use ieee.std_logic_1164.all;

entity lfudm is
  port (vxfvsaflwc : inout std_logic_vector(0 downto 2); jnwdaqx : out character);
end lfudm;

library ieee;
use ieee.std_logic_1164.all;

architecture pqgiqjree of lfudm is
  signal tgg : time;
  signal ybhppk : character;
  signal cxjfe : time;
  signal wnfizdqh : std_logic_vector(4 downto 4);
begin
  itilxl : entity work.ci
    port map (aaiqd => wnfizdqh, rkstxfyru => cxjfe, p => ybhppk);
  vzmhfohc : entity work.ci
    port map (aaiqd => wnfizdqh, rkstxfyru => tgg, p => jnwdaqx);
  
  -- Single-driven assignments
  jnwdaqx <= jnwdaqx;
  ybhppk <= jnwdaqx;
  
  -- Multi-driven assignments
  wnfizdqh <= "L";
  vxfvsaflwc <= vxfvsaflwc;
end pqgiqjree;

library ieee;
use ieee.std_logic_1164.all;

entity yjanqxmb is
  port (ngtsgbhu : linkage bit; bgihv : in integer; cz : in std_logic_vector(0 to 0));
end yjanqxmb;

library ieee;
use ieee.std_logic_1164.all;

architecture khwj of yjanqxmb is
  signal gk : time;
  signal kfxaq : character;
  signal krwnm : time;
  signal pwk : std_logic_vector(4 downto 4);
begin
  rerqkpwjwz : entity work.ci
    port map (aaiqd => pwk, rkstxfyru => krwnm, p => kfxaq);
  soryfva : entity work.ci
    port map (aaiqd => pwk, rkstxfyru => gk, p => kfxaq);
end khwj;

library ieee;
use ieee.std_logic_1164.all;

entity xqbc is
  port (wh : in std_logic_vector(3 downto 0); m : buffer boolean_vector(3 downto 0); fjvyx : in std_logic_vector(2 downto 1));
end xqbc;

library ieee;
use ieee.std_logic_1164.all;

architecture pqtjpaczt of xqbc is
  signal o : character;
  signal jil : std_logic_vector(0 downto 2);
  signal mzyomlcqwx : character;
  signal gjqvu : time;
  signal ljzf : std_logic_vector(4 downto 4);
  signal syjs : std_logic_vector(0 to 0);
  signal wycfc : integer;
  signal vahavytcxg : bit;
begin
  xtj : entity work.yjanqxmb
    port map (ngtsgbhu => vahavytcxg, bgihv => wycfc, cz => syjs);
  c : entity work.ci
    port map (aaiqd => ljzf, rkstxfyru => gjqvu, p => mzyomlcqwx);
  uz : entity work.lfudm
    port map (vxfvsaflwc => jil, jnwdaqx => o);
  
  -- Single-driven assignments
  m <= m;
  mzyomlcqwx <= o;
  wycfc <= 1_4_2_1;
  
  -- Multi-driven assignments
  syjs <= (others => '0');
  syjs <= (others => 'Z');
  syjs <= (others => '-');
end pqtjpaczt;



-- Seed after: 11142631820968467163,16188444798499499427
