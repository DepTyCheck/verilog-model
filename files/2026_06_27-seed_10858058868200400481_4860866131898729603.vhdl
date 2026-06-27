-- Seed: 10858058868200400481,4860866131898729603

entity agb is
  port (uriultsqa : linkage time_vector(4 to 4));
end agb;

architecture v of agb is
  
begin
  
end v;

entity g is
  port (xmco : out time);
end g;

architecture ymodiqjjvo of g is
  signal kyasb : time_vector(4 to 4);
  signal mhafbx : time_vector(4 to 4);
  signal sfxebuzg : time_vector(4 to 4);
begin
  yymrmdmlx : entity work.agb
    port map (uriultsqa => sfxebuzg);
  fpmmtfe : entity work.agb
    port map (uriultsqa => mhafbx);
  axizmiw : entity work.agb
    port map (uriultsqa => kyasb);
  
  -- Single-driven assignments
  xmco <= 1_1_0_4.3 ns;
end ymodiqjjvo;



-- Seed after: 9456806557236825488,4860866131898729603
