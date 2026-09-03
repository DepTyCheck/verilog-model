-- Seed: 14963988335386975462,11127274767545411571

entity efjioig is
  port (ajr : buffer integer_vector(4 to 4); unqzzyws : out real_vector(2 downto 1));
end efjioig;

architecture xf of efjioig is
  
begin
  -- Single-driven assignments
  unqzzyws <= (0020.011, 4204.3012);
  ajr <= ajr;
end xf;

entity chuqwl is
  port (nyfdpwr : inout bit; fxqny : out integer);
end chuqwl;

architecture lbvcclbfcn of chuqwl is
  signal ycx : real_vector(2 downto 1);
  signal puaptouti : integer_vector(4 to 4);
  signal dix : real_vector(2 downto 1);
  signal n : integer_vector(4 to 4);
  signal z : real_vector(2 downto 1);
  signal nyykdmzrx : integer_vector(4 to 4);
  signal sxnl : real_vector(2 downto 1);
  signal o : integer_vector(4 to 4);
begin
  ztub : entity work.efjioig
    port map (ajr => o, unqzzyws => sxnl);
  czgb : entity work.efjioig
    port map (ajr => nyykdmzrx, unqzzyws => z);
  npbxigsf : entity work.efjioig
    port map (ajr => n, unqzzyws => dix);
  jqzwlpt : entity work.efjioig
    port map (ajr => puaptouti, unqzzyws => ycx);
  
  -- Single-driven assignments
  fxqny <= 2#0_1_0_1#;
end lbvcclbfcn;



-- Seed after: 5214423183911343276,11127274767545411571
