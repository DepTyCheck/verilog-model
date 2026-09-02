-- Seed: 9483108322053525533,3400751927341804175

library ieee;
use ieee.std_logic_1164.all;

entity qe is
  port (tqod : inout character; pbpinnlcpa : inout bit_vector(0 downto 2); tub : linkage std_logic_vector(4 to 0));
end qe;

architecture yzdn of qe is
  
begin
  -- Single-driven assignments
  pbpinnlcpa <= (others => '0');
end yzdn;

entity nrwvyfd is
  port (rdo : inout time);
end nrwvyfd;

library ieee;
use ieee.std_logic_1164.all;

architecture unlamvu of nrwvyfd is
  signal feu : bit_vector(0 downto 2);
  signal oyueq : character;
  signal isqunvpq : bit_vector(0 downto 2);
  signal ikyeopi : character;
  signal quhhhmzvg : std_logic_vector(4 to 0);
  signal resvw : bit_vector(0 downto 2);
  signal zc : character;
  signal nnueedx : std_logic_vector(4 to 0);
  signal tsbyuzv : bit_vector(0 downto 2);
  signal cvynqikbyk : character;
begin
  mwcalggf : entity work.qe
    port map (tqod => cvynqikbyk, pbpinnlcpa => tsbyuzv, tub => nnueedx);
  syzxa : entity work.qe
    port map (tqod => zc, pbpinnlcpa => resvw, tub => quhhhmzvg);
  peqfqbbvfj : entity work.qe
    port map (tqod => ikyeopi, pbpinnlcpa => isqunvpq, tub => nnueedx);
  md : entity work.qe
    port map (tqod => oyueq, pbpinnlcpa => feu, tub => nnueedx);
  
  -- Multi-driven assignments
  nnueedx <= "";
  nnueedx <= (others => '0');
  quhhhmzvg <= nnueedx;
  nnueedx <= nnueedx;
end unlamvu;



-- Seed after: 7529084321949246700,3400751927341804175
