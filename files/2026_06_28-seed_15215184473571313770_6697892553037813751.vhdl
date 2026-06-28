-- Seed: 15215184473571313770,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity c is
  port (gsrfd : linkage std_logic_vector(0 to 1); pyf : out bit_vector(2 to 1); acop : linkage bit);
end c;

architecture snsixnjqk of c is
  
begin
  -- Single-driven assignments
  pyf <= (others => '0');
end snsixnjqk;

entity kobbtifdn is
  port (otplpvsdb : out bit_vector(2 downto 2); fpff : linkage real; oxeuum : linkage real);
end kobbtifdn;

library ieee;
use ieee.std_logic_1164.all;

architecture xswihb of kobbtifdn is
  signal kcjg : bit;
  signal jnhoz : bit_vector(2 to 1);
  signal lqg : std_logic_vector(0 to 1);
  signal zs : bit;
  signal bruyjpifi : bit_vector(2 to 1);
  signal lgebxwmh : bit;
  signal obvzmc : bit_vector(2 to 1);
  signal qwbne : std_logic_vector(0 to 1);
  signal lw : bit;
  signal tgtmbvor : bit_vector(2 to 1);
  signal zqztotdyb : std_logic_vector(0 to 1);
begin
  fgtbjdx : entity work.c
    port map (gsrfd => zqztotdyb, pyf => tgtmbvor, acop => lw);
  mmudpdmgg : entity work.c
    port map (gsrfd => qwbne, pyf => obvzmc, acop => lgebxwmh);
  ya : entity work.c
    port map (gsrfd => zqztotdyb, pyf => bruyjpifi, acop => zs);
  pllgasr : entity work.c
    port map (gsrfd => lqg, pyf => jnhoz, acop => kcjg);
  
  -- Single-driven assignments
  otplpvsdb <= (others => '1');
  
  -- Multi-driven assignments
  zqztotdyb <= "00";
  zqztotdyb <= "-Z";
  zqztotdyb <= "-H";
  lqg <= ('X', '-');
end xswihb;



-- Seed after: 8872257884166993346,6697892553037813751
