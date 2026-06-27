-- Seed: 7383613693604714091,4860866131898729603

entity odeezg is
  port (bdc : inout bit_vector(4 to 0));
end odeezg;

architecture exr of odeezg is
  
begin
  -- Single-driven assignments
  bdc <= (others => '0');
end exr;

entity cir is
  port (lrmbb : out integer);
end cir;

architecture eznu of cir is
  signal yqv : bit_vector(4 to 0);
  signal ssdc : bit_vector(4 to 0);
  signal mepzj : bit_vector(4 to 0);
begin
  gwqjwhctf : entity work.odeezg
    port map (bdc => mepzj);
  rqnyguogre : entity work.odeezg
    port map (bdc => ssdc);
  rlkzpcgs : entity work.odeezg
    port map (bdc => yqv);
  
  -- Single-driven assignments
  lrmbb <= 31200;
end eznu;



-- Seed after: 10858058868200400481,4860866131898729603
