-- Seed: 2485692144654141111,13479070923501788437

library ieee;
use ieee.std_logic_1164.all;

entity cmi is
  port (olzejr : linkage time; lamro : in std_logic; wrbgwg : buffer real; zzs : inout severity_level);
end cmi;

architecture pv of cmi is
  
begin
  
end pv;

entity p is
  port (utisdpryyv : buffer boolean);
end p;

library ieee;
use ieee.std_logic_1164.all;

architecture fb of p is
  signal arusadyl : severity_level;
  signal pmimv : real;
  signal hapigquznu : std_logic;
  signal rdzvyw : time;
  signal angui : severity_level;
  signal iwkopv : real;
  signal vteuiiaqzl : std_logic;
  signal vfxtxsxvc : time;
  signal m : severity_level;
  signal z : real;
  signal rzd : std_logic;
  signal ljm : time;
begin
  e : entity work.cmi
    port map (olzejr => ljm, lamro => rzd, wrbgwg => z, zzs => m);
  pr : entity work.cmi
    port map (olzejr => vfxtxsxvc, lamro => vteuiiaqzl, wrbgwg => iwkopv, zzs => angui);
  emmg : entity work.cmi
    port map (olzejr => rdzvyw, lamro => hapigquznu, wrbgwg => pmimv, zzs => arusadyl);
  
  -- Single-driven assignments
  utisdpryyv <= TRUE;
  
  -- Multi-driven assignments
  rzd <= '0';
  rzd <= 'X';
  rzd <= 'H';
end fb;

entity vhbvt is
  port (uqpszrybkk : out real);
end vhbvt;

library ieee;
use ieee.std_logic_1164.all;

architecture rq of vhbvt is
  signal flo : severity_level;
  signal i : real;
  signal hssdtkjozj : time;
  signal cjithgt : severity_level;
  signal yvwkikqxq : std_logic;
  signal fkbsrof : time;
begin
  mqnixed : entity work.cmi
    port map (olzejr => fkbsrof, lamro => yvwkikqxq, wrbgwg => uqpszrybkk, zzs => cjithgt);
  gduqnggwqz : entity work.cmi
    port map (olzejr => hssdtkjozj, lamro => yvwkikqxq, wrbgwg => i, zzs => flo);
  
  -- Multi-driven assignments
  yvwkikqxq <= '0';
  yvwkikqxq <= '0';
  yvwkikqxq <= 'W';
end rq;



-- Seed after: 12779875082022064898,13479070923501788437
