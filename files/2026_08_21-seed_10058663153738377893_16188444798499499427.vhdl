-- Seed: 10058663153738377893,16188444798499499427

entity xqfjyou is
  port (tjmxkepm : out time_vector(3 to 1); juqdh : buffer time_vector(3 to 3));
end xqfjyou;

architecture wozm of xqfjyou is
  
begin
  -- Single-driven assignments
  juqdh <= juqdh;
  tjmxkepm <= (others => 0 ns);
end wozm;

entity ofykp is
  port (mvnftl : buffer integer);
end ofykp;

architecture dtyxuu of ofykp is
  signal thixqamxxn : time_vector(3 to 3);
  signal ivgxczivem : time_vector(3 to 1);
  signal oscwvwpap : time_vector(3 to 3);
  signal fvjz : time_vector(3 to 1);
  signal lxlebyl : time_vector(3 to 3);
  signal ddayywpq : time_vector(3 to 1);
begin
  xbtdevd : entity work.xqfjyou
    port map (tjmxkepm => ddayywpq, juqdh => lxlebyl);
  g : entity work.xqfjyou
    port map (tjmxkepm => fvjz, juqdh => oscwvwpap);
  pmtipv : entity work.xqfjyou
    port map (tjmxkepm => ivgxczivem, juqdh => thixqamxxn);
end dtyxuu;



-- Seed after: 9399005949828882553,16188444798499499427
