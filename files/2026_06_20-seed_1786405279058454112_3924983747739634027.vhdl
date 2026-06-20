-- Seed: 1786405279058454112,3924983747739634027

entity bwwm is
  port (lgxk : in time; vji : buffer severity_level);
end bwwm;

architecture las of bwwm is
  
begin
  -- Single-driven assignments
  vji <= WARNING;
end las;

entity vtkvpcbgcw is
  port (ijipydyn : inout time_vector(1 to 4));
end vtkvpcbgcw;

architecture bzy of vtkvpcbgcw is
  signal oqcprdfdel : severity_level;
  signal x : severity_level;
  signal g : time;
  signal rufqj : severity_level;
  signal tmkpyi : severity_level;
  signal tna : time;
begin
  rtgksafrue : entity work.bwwm
    port map (lgxk => tna, vji => tmkpyi);
  spvnj : entity work.bwwm
    port map (lgxk => tna, vji => rufqj);
  qxbz : entity work.bwwm
    port map (lgxk => g, vji => x);
  bftglzluk : entity work.bwwm
    port map (lgxk => tna, vji => oqcprdfdel);
  
  -- Single-driven assignments
  ijipydyn <= (3 sec, 8#1_7_7_4.0_5_2_2_5# fs, 1 sec, 3020 ps);
end bzy;



-- Seed after: 5657578034649823862,3924983747739634027
