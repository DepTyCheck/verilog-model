-- Seed: 5460631902283811411,10594830431004325987

entity aiixw is
  port (wmf : buffer string(3 to 5));
end aiixw;

architecture x of aiixw is
  
begin
  -- Single-driven assignments
  wmf <= ('y', 'b', 'a');
end x;

entity envb is
  port (pxz : out real; xnah : out real);
end envb;

architecture s of envb is
  signal q : string(3 to 5);
  signal gkbzddj : string(3 to 5);
begin
  a : entity work.aiixw
    port map (wmf => gkbzddj);
  znc : entity work.aiixw
    port map (wmf => q);
  
  -- Single-driven assignments
  xnah <= xnah;
  pxz <= 2#1_0_0_1.0111#;
end s;



-- Seed after: 7892841640977493931,10594830431004325987
