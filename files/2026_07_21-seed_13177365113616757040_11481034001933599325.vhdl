-- Seed: 13177365113616757040,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity znm is
  port (bhgizsqlwr : in std_logic_vector(2 to 2); y : inout boolean; prsal : buffer real);
end znm;

architecture fkqcwom of znm is
  
begin
  -- Single-driven assignments
  y <= TRUE;
end fkqcwom;

library ieee;
use ieee.std_logic_1164.all;

entity xtpun is
  port (artnu : inout std_logic; e : buffer std_logic_vector(0 to 2));
end xtpun;

library ieee;
use ieee.std_logic_1164.all;

architecture aqkgfasb of xtpun is
  signal fqh : real;
  signal ke : boolean;
  signal sbwx : std_logic_vector(2 to 2);
  signal wlvjg : real;
  signal s : boolean;
  signal gb : std_logic_vector(2 to 2);
begin
  btsctkqyk : entity work.znm
    port map (bhgizsqlwr => gb, y => s, prsal => wlvjg);
  qxoyv : entity work.znm
    port map (bhgizsqlwr => sbwx, y => ke, prsal => fqh);
  
  -- Multi-driven assignments
  artnu <= artnu;
  e <= "1X0";
  e <= "HXH";
end aqkgfasb;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (hk : linkage integer; m : out severity_level; loahl : buffer std_logic_vector(1 downto 1); sav : buffer boolean_vector(4 to 4));
end a;

architecture tylwodp of a is
  signal tixtfjbp : real;
  signal oqkiarw : boolean;
  signal imabjiadso : real;
  signal vfxpigw : boolean;
begin
  woxzsxumez : entity work.znm
    port map (bhgizsqlwr => loahl, y => vfxpigw, prsal => imabjiadso);
  aqivb : entity work.znm
    port map (bhgizsqlwr => loahl, y => oqkiarw, prsal => tixtfjbp);
  
  -- Single-driven assignments
  sav <= sav;
  m <= ERROR;
  
  -- Multi-driven assignments
  loahl <= "0";
  loahl <= loahl;
end tylwodp;

entity ykjwyvaoj is
  port (tlgh : linkage integer; ybbgsoupv : out integer_vector(1 downto 1));
end ykjwyvaoj;

library ieee;
use ieee.std_logic_1164.all;

architecture yf of ykjwyvaoj is
  signal wqlyctbaba : boolean_vector(4 to 4);
  signal zgqgwwcazw : severity_level;
  signal sjbzoeha : integer;
  signal ttsy : boolean_vector(4 to 4);
  signal gb : std_logic_vector(1 downto 1);
  signal oyzgddxyhj : severity_level;
  signal lbg : integer;
begin
  dq : entity work.a
    port map (hk => lbg, m => oyzgddxyhj, loahl => gb, sav => ttsy);
  jh : entity work.a
    port map (hk => sjbzoeha, m => zgqgwwcazw, loahl => gb, sav => wqlyctbaba);
  
  -- Single-driven assignments
  ybbgsoupv <= (others => 2_2);
  
  -- Multi-driven assignments
  gb <= "1";
  gb <= (others => '-');
  gb <= gb;
  gb <= gb;
end yf;



-- Seed after: 16157428893629360568,11481034001933599325
