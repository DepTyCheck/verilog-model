-- Seed: 4257420808563810416,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ouktflv is
  port (omvoysvnz : inout std_logic_vector(3 downto 0); tbefed : out bit_vector(1 downto 2));
end ouktflv;

architecture san of ouktflv is
  
begin
  -- Single-driven assignments
  tbefed <= (others => '0');
end san;

entity fmmiltmu is
  port (z : buffer real);
end fmmiltmu;

library ieee;
use ieee.std_logic_1164.all;

architecture d of fmmiltmu is
  signal ihrmsouea : bit_vector(1 downto 2);
  signal cxpmww : std_logic_vector(3 downto 0);
  signal bkpek : bit_vector(1 downto 2);
  signal j : bit_vector(1 downto 2);
  signal xauqlem : std_logic_vector(3 downto 0);
  signal xscey : bit_vector(1 downto 2);
  signal ynvnu : std_logic_vector(3 downto 0);
begin
  zhieyjg : entity work.ouktflv
    port map (omvoysvnz => ynvnu, tbefed => xscey);
  odzbwugo : entity work.ouktflv
    port map (omvoysvnz => xauqlem, tbefed => j);
  e : entity work.ouktflv
    port map (omvoysvnz => ynvnu, tbefed => bkpek);
  isxovg : entity work.ouktflv
    port map (omvoysvnz => cxpmww, tbefed => ihrmsouea);
end d;

library ieee;
use ieee.std_logic_1164.all;

entity lygqjfmj is
  port (bezt : inout std_logic_vector(3 to 2); w : inout real; yflpfotw : inout severity_level; i : linkage bit);
end lygqjfmj;

library ieee;
use ieee.std_logic_1164.all;

architecture wx of lygqjfmj is
  signal jtdx : bit_vector(1 downto 2);
  signal n : bit_vector(1 downto 2);
  signal vryzqihgzb : bit_vector(1 downto 2);
  signal s : std_logic_vector(3 downto 0);
begin
  jywsgawj : entity work.ouktflv
    port map (omvoysvnz => s, tbefed => vryzqihgzb);
  tdvahirncx : entity work.ouktflv
    port map (omvoysvnz => s, tbefed => n);
  ry : entity work.ouktflv
    port map (omvoysvnz => s, tbefed => jtdx);
end wx;



-- Seed after: 13808800863086047616,14652815260262078753
