-- Seed: 4102961465249298213,4122021602305298647

entity e is
  port (ftyuj : linkage bit_vector(0 downto 4); dxvdmjszet : buffer time; udmepfw : in integer; agvcr : out string(5 to 2));
end e;

architecture t of e is
  
begin
  -- Single-driven assignments
  dxvdmjszet <= 4_2_0.3_3 ns;
  agvcr <= "";
end t;

entity jijizsnnpp is
  port (zqcyc : out boolean_vector(1 downto 3); hcqtypexha : in integer; ypy : out time; hbsvukcu : buffer bit);
end jijizsnnpp;

architecture robilzimn of jijizsnnpp is
  signal wnvjxz : string(5 to 2);
  signal opz : bit_vector(0 downto 4);
  signal bvlsvb : string(5 to 2);
  signal sc : integer;
  signal kpzqshmzd : time;
  signal hv : bit_vector(0 downto 4);
begin
  zllpjvcyc : entity work.e
    port map (ftyuj => hv, dxvdmjszet => kpzqshmzd, udmepfw => sc, agvcr => bvlsvb);
  byarpeu : entity work.e
    port map (ftyuj => opz, dxvdmjszet => ypy, udmepfw => hcqtypexha, agvcr => wnvjxz);
  
  -- Single-driven assignments
  hbsvukcu <= '1';
  zqcyc <= (others => TRUE);
  sc <= 8#7_7#;
end robilzimn;



-- Seed after: 1745132244989915963,4122021602305298647
