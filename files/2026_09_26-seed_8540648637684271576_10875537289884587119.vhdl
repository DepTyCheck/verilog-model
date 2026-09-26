-- Seed: 8540648637684271576,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity aeft is
  port (zhphawysxa : linkage std_logic; eadwrnp : inout time; qm : in time);
end aeft;

architecture as of aeft is
  
begin
  
end as;

library ieee;
use ieee.std_logic_1164.all;

entity z is
  port ( d : linkage std_logic_vector(4 downto 4)
  ; aogy : inout severity_level
  ; jdlqomrshx : in std_logic_vector(0 downto 2)
  ; qufftbl : inout real_vector(0 to 1)
  );
end z;

library ieee;
use ieee.std_logic_1164.all;

architecture zhvgmwkx of z is
  signal jvadavkjvh : time;
  signal bbpqhaess : std_logic;
  signal ioxkk : time;
  signal crai : time;
  signal wzigi : std_logic;
begin
  fbpnvto : entity work.aeft
    port map (zhphawysxa => wzigi, eadwrnp => crai, qm => ioxkk);
  bzeleq : entity work.aeft
    port map (zhphawysxa => bbpqhaess, eadwrnp => ioxkk, qm => jvadavkjvh);
  
  -- Single-driven assignments
  qufftbl <= qufftbl;
  
  -- Multi-driven assignments
  bbpqhaess <= '0';
  wzigi <= 'H';
  bbpqhaess <= 'L';
end zhvgmwkx;

entity duha is
  port (ze : in severity_level; qlwlgv : in string(4 downto 2));
end duha;

library ieee;
use ieee.std_logic_1164.all;

architecture mwwotnez of duha is
  signal yypat : time;
  signal wjx : std_logic;
  signal hoijm : real_vector(0 to 1);
  signal e : std_logic_vector(0 downto 2);
  signal ob : severity_level;
  signal hxlidy : std_logic_vector(4 downto 4);
  signal d : time;
  signal gzkzyvkc : std_logic;
  signal dekbd : time;
  signal zchi : time;
  signal aihutgadua : std_logic;
begin
  rw : entity work.aeft
    port map (zhphawysxa => aihutgadua, eadwrnp => zchi, qm => dekbd);
  jvvqzhr : entity work.aeft
    port map (zhphawysxa => gzkzyvkc, eadwrnp => d, qm => dekbd);
  ucl : entity work.z
    port map (d => hxlidy, aogy => ob, jdlqomrshx => e, qufftbl => hoijm);
  wlbhbdbpu : entity work.aeft
    port map (zhphawysxa => wjx, eadwrnp => dekbd, qm => yypat);
  
  -- Single-driven assignments
  yypat <= 2#100.10100# ns;
end mwwotnez;



-- Seed after: 5765288392541298136,10875537289884587119
