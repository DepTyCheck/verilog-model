-- Seed: 15074528680538965035,4860866131898729603

entity jkfwtwx is
  port (oq : buffer boolean_vector(3 to 3));
end jkfwtwx;

architecture bqdgldpu of jkfwtwx is
  
begin
  -- Single-driven assignments
  oq <= (others => FALSE);
end bqdgldpu;

entity gtyo is
  port (xclbjjq : out character);
end gtyo;

architecture oiwrakjz of gtyo is
  signal lcra : boolean_vector(3 to 3);
begin
  ptjgacq : entity work.jkfwtwx
    port map (oq => lcra);
end oiwrakjz;

entity jz is
  port (hvq : inout real);
end jz;

architecture agiciyfz of jz is
  signal cach : boolean_vector(3 to 3);
  signal zqs : boolean_vector(3 to 3);
begin
  gtarettcve : entity work.jkfwtwx
    port map (oq => zqs);
  hgtjysf : entity work.jkfwtwx
    port map (oq => cach);
  
  -- Single-driven assignments
  hvq <= 2#101.0#;
end agiciyfz;



-- Seed after: 12646893208368959686,4860866131898729603
