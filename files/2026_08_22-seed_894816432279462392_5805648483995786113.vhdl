-- Seed: 894816432279462392,5805648483995786113

entity vbfjch is
  port ( jorhj : in bit_vector(3 to 4)
  ; cfpcelpvkn : inout boolean_vector(0 to 1)
  ; g : inout integer_vector(3 downto 1)
  ; dpetqvjuif : out bit_vector(0 downto 4)
  );
end vbfjch;

architecture wr of vbfjch is
  
begin
  -- Single-driven assignments
  dpetqvjuif <= (others => '0');
  g <= g;
  cfpcelpvkn <= cfpcelpvkn;
end wr;

entity yejxpgpi is
  port (nyqb : in time);
end yejxpgpi;

architecture cvsh of yejxpgpi is
  signal e : bit_vector(0 downto 4);
  signal gumzxpr : integer_vector(3 downto 1);
  signal nhbh : boolean_vector(0 to 1);
  signal wiu : bit_vector(0 downto 4);
  signal plkfxoa : integer_vector(3 downto 1);
  signal cjipvwaj : boolean_vector(0 to 1);
  signal ce : bit_vector(0 downto 4);
  signal rehhr : integer_vector(3 downto 1);
  signal fud : boolean_vector(0 to 1);
  signal fsg : bit_vector(3 to 4);
begin
  mugxos : entity work.vbfjch
    port map (jorhj => fsg, cfpcelpvkn => fud, g => rehhr, dpetqvjuif => ce);
  wk : entity work.vbfjch
    port map (jorhj => fsg, cfpcelpvkn => cjipvwaj, g => plkfxoa, dpetqvjuif => wiu);
  qdfv : entity work.vbfjch
    port map (jorhj => fsg, cfpcelpvkn => nhbh, g => gumzxpr, dpetqvjuif => e);
  
  -- Single-driven assignments
  fsg <= fsg;
end cvsh;



-- Seed after: 5723386040591882636,5805648483995786113
