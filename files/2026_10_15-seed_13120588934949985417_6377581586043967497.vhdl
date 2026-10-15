-- Seed: 13120588934949985417,6377581586043967497

entity wh is
  port (itwt : buffer real);
end wh;

architecture btoucang of wh is
  
begin
  -- Single-driven assignments
  itwt <= 2#110.11111#;
end btoucang;

entity gnm is
  port (ber : in time; jokqmgeqj : out real; gbaakqvjah : inout real; r : buffer time_vector(2 to 0));
end gnm;

architecture ql of gnm is
  signal smpgv : real;
begin
  rzddw : entity work.wh
    port map (itwt => smpgv);
  
  -- Single-driven assignments
  r <= (others => 0 ns);
end ql;



-- Seed after: 10227877801665386739,6377581586043967497
