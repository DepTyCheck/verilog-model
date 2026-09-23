-- Seed: 2486897187934613081,8067602802092121131

entity jdjpzzxi is
  port (bbxvzwcbh : in real; jtg : in real; lprredycu : linkage integer);
end jdjpzzxi;

architecture spwqxba of jdjpzzxi is
  
begin
  
end spwqxba;

library ieee;
use ieee.std_logic_1164.all;

entity xvijbrh is
  port (oxj : buffer real; h : out std_logic_vector(3 to 0); k : in bit; zyufmnapoa : out time_vector(4 downto 1));
end xvijbrh;

architecture qqn of xvijbrh is
  signal drs : integer;
  signal rjiulnf : real;
  signal vk : integer;
  signal dvehhxispu : real;
  signal fdgv : integer;
  signal uucdyx : real;
  signal uvkxo : integer;
  signal njxkzjupsh : real;
  signal cveuevpvk : real;
begin
  av : entity work.jdjpzzxi
    port map (bbxvzwcbh => cveuevpvk, jtg => njxkzjupsh, lprredycu => uvkxo);
  bewhprr : entity work.jdjpzzxi
    port map (bbxvzwcbh => uucdyx, jtg => cveuevpvk, lprredycu => fdgv);
  xga : entity work.jdjpzzxi
    port map (bbxvzwcbh => njxkzjupsh, jtg => dvehhxispu, lprredycu => vk);
  opmvuklaqr : entity work.jdjpzzxi
    port map (bbxvzwcbh => cveuevpvk, jtg => rjiulnf, lprredycu => drs);
  
  -- Single-driven assignments
  rjiulnf <= oxj;
  uucdyx <= 2#0.1#;
  cveuevpvk <= 2#10101.0#;
  
  -- Multi-driven assignments
  h <= "";
end qqn;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (qsmgeoz : buffer real; anpxebsi : linkage std_logic; lupkunkdqc : out integer);
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture fb of q is
  signal tbyo : time_vector(4 downto 1);
  signal ucggyenyi : bit;
  signal omglmiolr : std_logic_vector(3 to 0);
  signal uofqf : real;
begin
  mbsck : entity work.xvijbrh
    port map (oxj => uofqf, h => omglmiolr, k => ucggyenyi, zyufmnapoa => tbyo);
  
  -- Single-driven assignments
  lupkunkdqc <= lupkunkdqc;
  qsmgeoz <= uofqf;
  ucggyenyi <= '0';
  
  -- Multi-driven assignments
  omglmiolr <= omglmiolr;
  omglmiolr <= "";
end fb;



-- Seed after: 16191301947300511268,8067602802092121131
