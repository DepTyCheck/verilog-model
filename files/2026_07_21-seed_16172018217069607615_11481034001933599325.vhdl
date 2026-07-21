-- Seed: 16172018217069607615,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity psmpix is
  port ( cmuskgdcnb : buffer bit_vector(0 downto 4)
  ; p : buffer std_logic
  ; amr : linkage bit_vector(2 downto 2)
  ; qacmlyen : linkage std_logic_vector(2 downto 2)
  );
end psmpix;

architecture sz of psmpix is
  
begin
  -- Single-driven assignments
  cmuskgdcnb <= (others => '0');
  
  -- Multi-driven assignments
  p <= p;
  p <= '0';
  p <= p;
end sz;

entity r is
  port (ble : buffer character);
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture nrtgnjar of r is
  signal gwprzgxhs : bit_vector(2 downto 2);
  signal zlegewjvr : bit_vector(0 downto 4);
  signal jylwmzcga : std_logic_vector(2 downto 2);
  signal svgne : bit_vector(2 downto 2);
  signal majqyav : std_logic;
  signal iktgfik : bit_vector(0 downto 4);
  signal wpuvuy : std_logic_vector(2 downto 2);
  signal vh : bit_vector(2 downto 2);
  signal jfod : std_logic;
  signal rlmjaxvcs : bit_vector(0 downto 4);
begin
  mhybs : entity work.psmpix
    port map (cmuskgdcnb => rlmjaxvcs, p => jfod, amr => vh, qacmlyen => wpuvuy);
  syzbu : entity work.psmpix
    port map (cmuskgdcnb => iktgfik, p => majqyav, amr => svgne, qacmlyen => jylwmzcga);
  lt : entity work.psmpix
    port map (cmuskgdcnb => zlegewjvr, p => jfod, amr => gwprzgxhs, qacmlyen => jylwmzcga);
  
  -- Single-driven assignments
  ble <= 't';
end nrtgnjar;



-- Seed after: 12048173343533210597,11481034001933599325
