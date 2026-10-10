-- Seed: 14816310991190860906,511364357853360275

library ieee;
use ieee.std_logic_1164.all;

entity yfvg is
  port (p : linkage std_logic; knme : in std_logic_vector(3 downto 4); hakdqu : linkage bit; v : linkage integer);
end yfvg;

architecture yscylgs of yfvg is
  
begin
  
end yscylgs;

library ieee;
use ieee.std_logic_1164.all;

entity ltaklcna is
  port (gbjtrx : in std_logic_vector(2 to 0); b : buffer std_logic; iwtozwnk : inout real; xthuun : buffer bit);
end ltaklcna;

library ieee;
use ieee.std_logic_1164.all;

architecture kppcw of ltaklcna is
  signal pcmksj : integer;
  signal xtoocki : bit;
  signal jfmwneo : std_logic;
  signal cufcha : integer;
  signal sahyjrch : bit;
  signal oicessw : std_logic_vector(3 downto 4);
  signal iz : integer;
  signal iqs : std_logic_vector(3 downto 4);
  signal cfemcayfb : integer;
  signal ln : bit;
  signal kowczcpy : std_logic_vector(3 downto 4);
  signal tdhoapoifb : std_logic;
begin
  oh : entity work.yfvg
    port map (p => tdhoapoifb, knme => kowczcpy, hakdqu => ln, v => cfemcayfb);
  qfzloiq : entity work.yfvg
    port map (p => b, knme => iqs, hakdqu => xthuun, v => iz);
  iu : entity work.yfvg
    port map (p => tdhoapoifb, knme => oicessw, hakdqu => sahyjrch, v => cufcha);
  sfzlct : entity work.yfvg
    port map (p => jfmwneo, knme => iqs, hakdqu => xtoocki, v => pcmksj);
  
  -- Single-driven assignments
  iwtozwnk <= 2.2_3_4_1_0;
  
  -- Multi-driven assignments
  oicessw <= (others => '0');
  tdhoapoifb <= b;
end kppcw;

entity rjgaqnpsy is
  port (hhjbbte : in real; banswrejm : buffer boolean_vector(4 downto 3));
end rjgaqnpsy;

library ieee;
use ieee.std_logic_1164.all;

architecture e of rjgaqnpsy is
  signal itw : integer;
  signal txxbm : bit;
  signal nrwplxur : std_logic_vector(3 downto 4);
  signal ptnzzlwe : integer;
  signal oawwap : bit;
  signal oxp : bit;
  signal pjkr : real;
  signal sokatua : std_logic;
  signal bcj : std_logic_vector(3 downto 4);
begin
  aiu : entity work.ltaklcna
    port map (gbjtrx => bcj, b => sokatua, iwtozwnk => pjkr, xthuun => oxp);
  afyv : entity work.yfvg
    port map (p => sokatua, knme => bcj, hakdqu => oawwap, v => ptnzzlwe);
  tshhau : entity work.yfvg
    port map (p => sokatua, knme => nrwplxur, hakdqu => txxbm, v => itw);
  
  -- Single-driven assignments
  banswrejm <= (TRUE, FALSE);
end e;

entity dhphci is
  port (kzom : in real);
end dhphci;

library ieee;
use ieee.std_logic_1164.all;

architecture x of dhphci is
  signal s : integer;
  signal c : bit;
  signal jiiuiv : std_logic;
  signal pwld : integer;
  signal oagaylcyyf : bit;
  signal aqxkwhfrn : std_logic_vector(3 downto 4);
  signal xuhcl : std_logic;
  signal wwwwx : boolean_vector(4 downto 3);
begin
  qdjsgqs : entity work.rjgaqnpsy
    port map (hhjbbte => kzom, banswrejm => wwwwx);
  tvykwhngm : entity work.yfvg
    port map (p => xuhcl, knme => aqxkwhfrn, hakdqu => oagaylcyyf, v => pwld);
  rzorycn : entity work.yfvg
    port map (p => jiiuiv, knme => aqxkwhfrn, hakdqu => c, v => s);
  
  -- Multi-driven assignments
  xuhcl <= '0';
end x;



-- Seed after: 1210684169311063463,511364357853360275
