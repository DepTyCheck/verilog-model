-- Seed: 8891592939144195947,662889661651915549

entity wkqwpb is
  port (mev : in time; fgefshdho : buffer bit_vector(2 downto 4));
end wkqwpb;

architecture zygsf of wkqwpb is
  
begin
  
end zygsf;

entity avione is
  port (vg : inout integer_vector(4 to 1));
end avione;

architecture xagae of avione is
  signal qyy : bit_vector(2 downto 4);
  signal hj : bit_vector(2 downto 4);
  signal ygqqnsx : time;
  signal tbzg : bit_vector(2 downto 4);
  signal nnq : bit_vector(2 downto 4);
  signal p : time;
begin
  sthcytjsyf : entity work.wkqwpb
    port map (mev => p, fgefshdho => nnq);
  ayofpdw : entity work.wkqwpb
    port map (mev => p, fgefshdho => tbzg);
  vvqss : entity work.wkqwpb
    port map (mev => ygqqnsx, fgefshdho => hj);
  rh : entity work.wkqwpb
    port map (mev => p, fgefshdho => qyy);
  
  -- Single-driven assignments
  vg <= (others => 0);
  ygqqnsx <= p;
  p <= 1.2_2_3_1_4 us;
end xagae;



-- Seed after: 11731651461547388624,662889661651915549
