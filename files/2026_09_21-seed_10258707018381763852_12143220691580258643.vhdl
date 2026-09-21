-- Seed: 10258707018381763852,12143220691580258643

library ieee;
use ieee.std_logic_1164.all;

entity riayjwulda is
  port (ebuear : inout time; btbxyzakz : linkage std_logic_vector(2 downto 2); kkrtgyyvd : inout real_vector(1 to 0); pzigboh : buffer time);
end riayjwulda;

architecture qkxuv of riayjwulda is
  
begin
  -- Single-driven assignments
  ebuear <= pzigboh;
  kkrtgyyvd <= (others => 0.0);
  pzigboh <= ebuear;
end qkxuv;

library ieee;
use ieee.std_logic_1164.all;

entity zjq is
  port (lhhc : inout std_logic; kuyoohlhbe : linkage bit_vector(0 to 4); ldqvbmk : linkage std_logic_vector(4 to 2));
end zjq;

library ieee;
use ieee.std_logic_1164.all;

architecture qg of zjq is
  signal glhuvnch : time;
  signal iiixrimqpw : real_vector(1 to 0);
  signal quuldgrnor : std_logic_vector(2 downto 2);
  signal unmqwcbasr : time;
  signal lbpwr : time;
  signal wokskfzvgc : real_vector(1 to 0);
  signal aqf : time;
  signal udljded : time;
  signal n : real_vector(1 to 0);
  signal mwr : time;
  signal luxhj : time;
  signal ojwdevbuim : real_vector(1 to 0);
  signal ngj : std_logic_vector(2 downto 2);
  signal jh : time;
begin
  huj : entity work.riayjwulda
    port map (ebuear => jh, btbxyzakz => ngj, kkrtgyyvd => ojwdevbuim, pzigboh => luxhj);
  p : entity work.riayjwulda
    port map (ebuear => mwr, btbxyzakz => ngj, kkrtgyyvd => n, pzigboh => udljded);
  sksszm : entity work.riayjwulda
    port map (ebuear => aqf, btbxyzakz => ngj, kkrtgyyvd => wokskfzvgc, pzigboh => lbpwr);
  qlzulj : entity work.riayjwulda
    port map (ebuear => unmqwcbasr, btbxyzakz => quuldgrnor, kkrtgyyvd => iiixrimqpw, pzigboh => glhuvnch);
  
  -- Multi-driven assignments
  quuldgrnor <= ngj;
  lhhc <= 'H';
  quuldgrnor <= (others => 'H');
  lhhc <= lhhc;
end qg;



-- Seed after: 4267341607679330341,12143220691580258643
