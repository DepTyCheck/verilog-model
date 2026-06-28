-- Seed: 6851016352688713668,6697892553037813751

entity axafeaabn is
  port (yici : linkage bit_vector(1 downto 4); t : out string(2 downto 5); pk : out integer_vector(4 downto 3); slavj : buffer bit);
end axafeaabn;

architecture j of axafeaabn is
  
begin
  -- Single-driven assignments
  slavj <= '1';
  t <= (others => ' ');
  pk <= (16#A_D_F_C_D#, 4_0_3_4);
end j;

entity nh is
  port (lug : inout integer_vector(0 to 0));
end nh;

architecture nexdwxiyh of nh is
  signal flisssa : bit;
  signal rfsdh : integer_vector(4 downto 3);
  signal imsw : string(2 downto 5);
  signal ou : bit_vector(1 downto 4);
  signal ojsqpiyk : bit;
  signal gfn : integer_vector(4 downto 3);
  signal gqfmlxithr : string(2 downto 5);
  signal vulvqgokt : bit_vector(1 downto 4);
  signal eheiajay : bit;
  signal xohmth : integer_vector(4 downto 3);
  signal tjogr : string(2 downto 5);
  signal uhmvwzzi : bit_vector(1 downto 4);
begin
  w : entity work.axafeaabn
    port map (yici => uhmvwzzi, t => tjogr, pk => xohmth, slavj => eheiajay);
  fcpgkyd : entity work.axafeaabn
    port map (yici => vulvqgokt, t => gqfmlxithr, pk => gfn, slavj => ojsqpiyk);
  izjsjafle : entity work.axafeaabn
    port map (yici => ou, t => imsw, pk => rfsdh, slavj => flisssa);
  
  -- Single-driven assignments
  lug <= (others => 4);
end nexdwxiyh;



-- Seed after: 17256237377937645229,6697892553037813751
