-- Seed: 5056559480119118934,4404421571376382767

entity lq is
  port (z : in severity_level; w : in bit_vector(2 to 1));
end lq;

architecture uiu of lq is
  
begin
  
end uiu;

entity oo is
  port (txumsgspw : buffer time_vector(0 downto 2); ftugxqs : in severity_level);
end oo;

architecture qvwok of oo is
  signal vpofo : bit_vector(2 to 1);
  signal t : severity_level;
begin
  zt : entity work.lq
    port map (z => t, w => vpofo);
  
  -- Single-driven assignments
  t <= ERROR;
  vpofo <= vpofo;
  txumsgspw <= (others => 0 ns);
end qvwok;



-- Seed after: 14285346957396349740,4404421571376382767
