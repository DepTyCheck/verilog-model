-- Seed: 13364420786456632298,13694093582652240945

entity bjo is
  port (buvroorst : inout time);
end bjo;

architecture h of bjo is
  
begin
  -- Single-driven assignments
  buvroorst <= 2#0_0_0.1010# ns;
end h;



-- Seed after: 7567151361298271360,13694093582652240945
