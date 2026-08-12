-- Seed: 14193622041319982556,8412319452373742525

entity rez is
  port (ifuprlclb : out time_vector(1 to 3));
end rez;

architecture zpjrugjwq of rez is
  
begin
  -- Single-driven assignments
  ifuprlclb <= (0 min, 2#1110# ns, 2_3_3 fs);
end zpjrugjwq;

entity np is
  port (anrfsplv : in real);
end np;

architecture eysoguv of np is
  signal ovynw : time_vector(1 to 3);
  signal vrudobwlxg : time_vector(1 to 3);
  signal bo : time_vector(1 to 3);
  signal mhhalvxe : time_vector(1 to 3);
begin
  mlakx : entity work.rez
    port map (ifuprlclb => mhhalvxe);
  epyexne : entity work.rez
    port map (ifuprlclb => bo);
  qqlwolzsg : entity work.rez
    port map (ifuprlclb => vrudobwlxg);
  hukt : entity work.rez
    port map (ifuprlclb => ovynw);
end eysoguv;



-- Seed after: 614896115677136780,8412319452373742525
