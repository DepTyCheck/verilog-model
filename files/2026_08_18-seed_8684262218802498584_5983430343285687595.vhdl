-- Seed: 8684262218802498584,5983430343285687595

entity gsmr is
  port (jcs : out real; m : buffer real);
end gsmr;

architecture cr of gsmr is
  
begin
  -- Single-driven assignments
  m <= m;
  jcs <= m;
end cr;



-- Seed after: 1757342902131598579,5983430343285687595
