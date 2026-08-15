-- Seed: 14885789182771732912,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity oqwkvxh is
  port (j : inout std_logic_vector(1 downto 2); mmdyuxvts : out bit; yroxwn : out std_logic; wqkowzydd : inout bit);
end oqwkvxh;

architecture l of oqwkvxh is
  
begin
  -- Single-driven assignments
  mmdyuxvts <= wqkowzydd;
  wqkowzydd <= wqkowzydd;
  
  -- Multi-driven assignments
  yroxwn <= 'U';
end l;

entity dpyrav is
  port (by : inout integer; ve : buffer bit; fzzbg : buffer real);
end dpyrav;

library ieee;
use ieee.std_logic_1164.all;

architecture lzlzc of dpyrav is
  signal xpac : bit;
  signal gojplshcoq : std_logic;
  signal lfwb : bit;
  signal atrxkv : std_logic_vector(1 downto 2);
  signal zbanfussgw : bit;
  signal cu : bit;
  signal sqvmtwyoic : std_logic;
  signal l : bit;
  signal ght : std_logic_vector(1 downto 2);
begin
  gadszuyhns : entity work.oqwkvxh
    port map (j => ght, mmdyuxvts => l, yroxwn => sqvmtwyoic, wqkowzydd => cu);
  iaqcoool : entity work.oqwkvxh
    port map (j => ght, mmdyuxvts => zbanfussgw, yroxwn => sqvmtwyoic, wqkowzydd => ve);
  ofmoqakasl : entity work.oqwkvxh
    port map (j => atrxkv, mmdyuxvts => lfwb, yroxwn => gojplshcoq, wqkowzydd => xpac);
  
  -- Multi-driven assignments
  gojplshcoq <= sqvmtwyoic;
  ght <= ght;
end lzlzc;



-- Seed after: 2222930798898455420,2230106469645304029
