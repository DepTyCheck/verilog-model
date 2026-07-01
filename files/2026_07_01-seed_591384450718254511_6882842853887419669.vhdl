-- Seed: 591384450718254511,6882842853887419669

entity ujqxghr is
  port (uy : inout string(2 downto 3));
end ujqxghr;

architecture wjtmibxeh of ujqxghr is
  
begin
  -- Single-driven assignments
  uy <= (others => ' ');
end wjtmibxeh;

entity gklrg is
  port (aafpjhaaby : buffer bit_vector(0 to 4); qcybfeoua : inout real);
end gklrg;

architecture divabps of gklrg is
  signal auihl : string(2 downto 3);
  signal foevdypzu : string(2 downto 3);
  signal br : string(2 downto 3);
begin
  bbrvunr : entity work.ujqxghr
    port map (uy => br);
  iqdjfhc : entity work.ujqxghr
    port map (uy => foevdypzu);
  whacgfyfgb : entity work.ujqxghr
    port map (uy => auihl);
end divabps;

entity jdfordc is
  port (etqmufqzs : out integer);
end jdfordc;

architecture qoqy of jdfordc is
  signal mlgn : string(2 downto 3);
  signal fdmghv : string(2 downto 3);
  signal oomqq : string(2 downto 3);
begin
  zjitkorlo : entity work.ujqxghr
    port map (uy => oomqq);
  deivbtmup : entity work.ujqxghr
    port map (uy => fdmghv);
  wsdd : entity work.ujqxghr
    port map (uy => mlgn);
  
  -- Single-driven assignments
  etqmufqzs <= 16#F_D_C_9#;
end qoqy;

library ieee;
use ieee.std_logic_1164.all;

entity nxcqu is
  port (mnsr : out bit; ibcseydn : out std_logic; p : buffer std_logic_vector(1 to 3));
end nxcqu;

architecture hzkpc of nxcqu is
  signal vw : real;
  signal zfkxy : bit_vector(0 to 4);
  signal rnitvg : real;
  signal vxyyrhzp : bit_vector(0 to 4);
  signal gumsblpfc : string(2 downto 3);
  signal x : string(2 downto 3);
begin
  mqygxt : entity work.ujqxghr
    port map (uy => x);
  nkmuibul : entity work.ujqxghr
    port map (uy => gumsblpfc);
  fdw : entity work.gklrg
    port map (aafpjhaaby => vxyyrhzp, qcybfeoua => rnitvg);
  iusfsosxw : entity work.gklrg
    port map (aafpjhaaby => zfkxy, qcybfeoua => vw);
  
  -- Multi-driven assignments
  p <= ('H', 'L', 'Z');
  p <= ('U', '0', 'H');
  p <= "00W";
end hzkpc;



-- Seed after: 3205550736542293011,6882842853887419669
