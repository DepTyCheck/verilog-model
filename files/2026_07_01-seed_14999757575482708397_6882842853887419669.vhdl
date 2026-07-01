-- Seed: 14999757575482708397,6882842853887419669

entity ndfftkd is
  port (tqtdkzrj : buffer bit; alud : buffer time);
end ndfftkd;

architecture kqffsrhx of ndfftkd is
  
begin
  -- Single-driven assignments
  alud <= 8#2_4_5_0.3# ms;
  tqtdkzrj <= '0';
end kqffsrhx;

entity bssfrpsdi is
  port (ckz : buffer bit_vector(3 to 0));
end bssfrpsdi;

architecture ccswvdpuxd of bssfrpsdi is
  signal zlwovck : time;
  signal aetb : bit;
  signal uoll : time;
  signal opftykrn : bit;
  signal tkrx : time;
  signal itkyjcyk : bit;
  signal ap : time;
  signal so : bit;
begin
  lyrgdx : entity work.ndfftkd
    port map (tqtdkzrj => so, alud => ap);
  tdliu : entity work.ndfftkd
    port map (tqtdkzrj => itkyjcyk, alud => tkrx);
  dpfbo : entity work.ndfftkd
    port map (tqtdkzrj => opftykrn, alud => uoll);
  ikkbdtktj : entity work.ndfftkd
    port map (tqtdkzrj => aetb, alud => zlwovck);
  
  -- Single-driven assignments
  ckz <= (others => '0');
end ccswvdpuxd;



-- Seed after: 6805979291510654343,6882842853887419669
