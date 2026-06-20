-- Seed: 5973133491175204209,17924494779688682807

entity f is
  port (psu : buffer real; ir : in real);
end f;

architecture gxqfp of f is
  
begin
  -- Single-driven assignments
  psu <= 16#4.6_C#;
end gxqfp;

library ieee;
use ieee.std_logic_1164.all;

entity fl is
  port (khvngdx : in std_logic_vector(3 to 1); e : in integer);
end fl;

architecture tiqwpkq of fl is
  signal zipteiuie : real;
  signal zkdmr : real;
  signal deiw : real;
  signal wetqq : real;
  signal iirpk : real;
begin
  c : entity work.f
    port map (psu => iirpk, ir => wetqq);
  rjs : entity work.f
    port map (psu => deiw, ir => iirpk);
  tdpg : entity work.f
    port map (psu => wetqq, ir => zkdmr);
  kj : entity work.f
    port map (psu => zipteiuie, ir => iirpk);
  
  -- Single-driven assignments
  zkdmr <= 2#111.1_1_1#;
end tiqwpkq;



-- Seed after: 13735191889870674913,17924494779688682807
