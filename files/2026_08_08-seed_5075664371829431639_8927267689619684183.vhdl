-- Seed: 5075664371829431639,8927267689619684183

library ieee;
use ieee.std_logic_1164.all;

entity wlvcn is
  port (bprdf : linkage time; uiwahlm : buffer real; jm : out std_logic_vector(2 to 2); bbyanbpi : in real);
end wlvcn;

architecture gtsfawunhu of wlvcn is
  
begin
  -- Single-driven assignments
  uiwahlm <= bbyanbpi;
  
  -- Multi-driven assignments
  jm <= (others => 'H');
end gtsfawunhu;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (rhhgv : out std_logic_vector(1 downto 3); bekwkia : buffer severity_level; xcdnienyl : out integer; ggtahoxg : inout bit);
end k;

library ieee;
use ieee.std_logic_1164.all;

architecture oxhpzjm of k is
  signal ul : real;
  signal ictw : std_logic_vector(2 to 2);
  signal pyiqbeh : time;
  signal pe : real;
  signal xjazlbeha : real;
  signal m : time;
  signal zg : real;
  signal xhdwjkfk : std_logic_vector(2 to 2);
  signal scbiemwmrw : real;
  signal a : time;
begin
  ximvr : entity work.wlvcn
    port map (bprdf => a, uiwahlm => scbiemwmrw, jm => xhdwjkfk, bbyanbpi => zg);
  uihufly : entity work.wlvcn
    port map (bprdf => m, uiwahlm => xjazlbeha, jm => xhdwjkfk, bbyanbpi => pe);
  vsomctxarp : entity work.wlvcn
    port map (bprdf => pyiqbeh, uiwahlm => pe, jm => ictw, bbyanbpi => ul);
  
  -- Multi-driven assignments
  rhhgv <= rhhgv;
  rhhgv <= (others => '0');
  ictw <= "H";
end oxhpzjm;

library ieee;
use ieee.std_logic_1164.all;

entity kyxyn is
  port (exaqkmntoj : inout real; vpmm : inout std_logic);
end kyxyn;

library ieee;
use ieee.std_logic_1164.all;

architecture pbvaa of kyxyn is
  signal ecbnlryxh : std_logic_vector(2 to 2);
  signal ep : time;
begin
  sacqfprf : entity work.wlvcn
    port map (bprdf => ep, uiwahlm => exaqkmntoj, jm => ecbnlryxh, bbyanbpi => exaqkmntoj);
  
  -- Multi-driven assignments
  vpmm <= vpmm;
  vpmm <= vpmm;
  vpmm <= '1';
  vpmm <= vpmm;
end pbvaa;

entity bmoldnt is
  port (qopcnljb : out real; wieg : out real);
end bmoldnt;

library ieee;
use ieee.std_logic_1164.all;

architecture hnmh of bmoldnt is
  signal u : bit;
  signal etai : integer;
  signal nvxaopj : severity_level;
  signal njlmjkhqy : std_logic_vector(1 downto 3);
begin
  onzvob : entity work.k
    port map (rhhgv => njlmjkhqy, bekwkia => nvxaopj, xcdnienyl => etai, ggtahoxg => u);
  
  -- Single-driven assignments
  wieg <= 2#01.0010#;
  qopcnljb <= wieg;
  
  -- Multi-driven assignments
  njlmjkhqy <= (others => '0');
  njlmjkhqy <= njlmjkhqy;
  njlmjkhqy <= njlmjkhqy;
end hnmh;



-- Seed after: 10195184539228347580,8927267689619684183
