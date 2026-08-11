-- Seed: 11136996892360587036,10594830431004325987

entity lnwvvff is
  port (ufuyngey : in boolean_vector(4 to 2));
end lnwvvff;

architecture dw of lnwvvff is
  
begin
  
end dw;

entity djk is
  port (ckxgbhsu : in integer);
end djk;

architecture yjmxpeil of djk is
  signal kupms : boolean_vector(4 to 2);
  signal qwojkze : boolean_vector(4 to 2);
  signal licgcq : boolean_vector(4 to 2);
begin
  bwhl : entity work.lnwvvff
    port map (ufuyngey => licgcq);
  sp : entity work.lnwvvff
    port map (ufuyngey => qwojkze);
  qen : entity work.lnwvvff
    port map (ufuyngey => licgcq);
  rxirmvg : entity work.lnwvvff
    port map (ufuyngey => kupms);
  
  -- Single-driven assignments
  licgcq <= (others => TRUE);
  qwojkze <= licgcq;
  kupms <= kupms;
end yjmxpeil;



-- Seed after: 12682540408158904202,10594830431004325987
