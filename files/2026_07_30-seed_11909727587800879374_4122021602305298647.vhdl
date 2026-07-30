-- Seed: 11909727587800879374,4122021602305298647

entity uvfdmea is
  port (usc : out real);
end uvfdmea;

architecture oesmf of uvfdmea is
  
begin
  -- Single-driven assignments
  usc <= 2#1001.1110#;
end oesmf;

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (au : in std_logic);
end f;

architecture ryelzexws of f is
  signal esvwngdk : real;
  signal wfgmkhqzqc : real;
  signal jp : real;
  signal lxv : real;
begin
  xhyqn : entity work.uvfdmea
    port map (usc => lxv);
  hlh : entity work.uvfdmea
    port map (usc => jp);
  ttvzets : entity work.uvfdmea
    port map (usc => wfgmkhqzqc);
  pbmsuep : entity work.uvfdmea
    port map (usc => esvwngdk);
end ryelzexws;

library ieee;
use ieee.std_logic_1164.all;

entity k is
  port (fzy : buffer time; hav : inout std_logic);
end k;

architecture dlyiodku of k is
  signal namkvachg : real;
begin
  l : entity work.uvfdmea
    port map (usc => namkvachg);
  
  -- Single-driven assignments
  fzy <= 1 hr;
end dlyiodku;



-- Seed after: 3057089892494003866,4122021602305298647
