-- Seed: 16573883404647920086,6290177331721581829

use std.reflection.all;

entity xyhgneg is
  port (qybenven : inout floating_value_mirror; atcektmzvm : in time; w : inout bit_vector(0 downto 1));
end xyhgneg;

architecture cap of xyhgneg is
  
begin
  -- Single-driven assignments
  w <= (others => '0');
end cap;

entity aiuumz is
  port (p : in real);
end aiuumz;

use std.reflection.all;

architecture bjh of aiuumz is
  signal mtdrt : bit_vector(0 downto 1);
  shared variable hclhdxae : floating_value_mirror;
  signal glt : bit_vector(0 downto 1);
  signal wfg : time;
  shared variable mfpzqck : floating_value_mirror;
begin
  fyspjqe : entity work.xyhgneg
    port map (qybenven => mfpzqck, atcektmzvm => wfg, w => glt);
  rlg : entity work.xyhgneg
    port map (qybenven => hclhdxae, atcektmzvm => wfg, w => mtdrt);
  
  -- Single-driven assignments
  wfg <= wfg;
end bjh;



-- Seed after: 2234972736713358647,6290177331721581829
