-- Seed: 11879692938080692028,12359743974512393525

entity flmo is
  port (rmgqfcnz : inout integer_vector(3 to 2); taaj : inout severity_level; imxo : in bit; vxnqq : buffer time);
end flmo;

architecture uwtm of flmo is
  
begin
  
end uwtm;

entity ujyhr is
  port (ynavjker : inout integer);
end ujyhr;

architecture x of ujyhr is
  signal zlenvt : time;
  signal ufnjwywf : severity_level;
  signal qhuuc : integer_vector(3 to 2);
  signal jvq : time;
  signal kggmn : severity_level;
  signal vkgsrzvllb : integer_vector(3 to 2);
  signal vedpizuho : time;
  signal offko : bit;
  signal xuedcr : severity_level;
  signal z : integer_vector(3 to 2);
begin
  szysc : entity work.flmo
    port map (rmgqfcnz => z, taaj => xuedcr, imxo => offko, vxnqq => vedpizuho);
  wdrewqe : entity work.flmo
    port map (rmgqfcnz => vkgsrzvllb, taaj => kggmn, imxo => offko, vxnqq => jvq);
  idybr : entity work.flmo
    port map (rmgqfcnz => qhuuc, taaj => ufnjwywf, imxo => offko, vxnqq => zlenvt);
end x;

library ieee;
use ieee.std_logic_1164.all;

entity tyhdc is
  port (n : buffer real; ybexsm : inout std_logic_vector(3 to 4); aqfipxug : in time; qgh : buffer time);
end tyhdc;

architecture p of tyhdc is
  signal pjillhmeto : bit;
  signal s : severity_level;
  signal lnpgj : integer_vector(3 to 2);
begin
  a : entity work.flmo
    port map (rmgqfcnz => lnpgj, taaj => s, imxo => pjillhmeto, vxnqq => qgh);
  
  -- Single-driven assignments
  n <= 2#11100.1_1#;
  pjillhmeto <= '1';
  
  -- Multi-driven assignments
  ybexsm <= "HZ";
end p;

entity n is
  port (uexrsgwpb : out severity_level; xrwte : inout severity_level; jzmzliwgue : in character);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture ue of n is
  signal ptvnnr : integer;
  signal kk : time;
  signal frhf : time;
  signal jr : std_logic_vector(3 to 4);
  signal eiofzw : real;
  signal aetwzb : integer;
begin
  wmocb : entity work.ujyhr
    port map (ynavjker => aetwzb);
  iqoukuaay : entity work.tyhdc
    port map (n => eiofzw, ybexsm => jr, aqfipxug => frhf, qgh => kk);
  e : entity work.ujyhr
    port map (ynavjker => ptvnnr);
  
  -- Single-driven assignments
  xrwte <= WARNING;
  uexrsgwpb <= xrwte;
  
  -- Multi-driven assignments
  jr <= jr;
  jr <= "HX";
  jr <= ('H', 'Z');
end ue;



-- Seed after: 11463571017689287456,12359743974512393525
