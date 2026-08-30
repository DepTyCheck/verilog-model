-- Seed: 5201093515290329247,4080032123900078489

entity lyrlca is
  port (diu : out severity_level);
end lyrlca;

architecture hbodmjyjh of lyrlca is
  
begin
  
end hbodmjyjh;

entity fex is
  port (keysykqa : out time_vector(0 downto 3));
end fex;

architecture jo of fex is
  signal kchbhpgkc : severity_level;
  signal ejmx : severity_level;
begin
  thnvv : entity work.lyrlca
    port map (diu => ejmx);
  toyyoh : entity work.lyrlca
    port map (diu => kchbhpgkc);
end jo;

entity jjjhhectd is
  port (eixc : in boolean; h : buffer real);
end jjjhhectd;

architecture yay of jjjhhectd is
  signal akihe : time_vector(0 downto 3);
  signal bojtmvk : time_vector(0 downto 3);
  signal voumegqrv : severity_level;
  signal rd : severity_level;
begin
  gegpixhdo : entity work.lyrlca
    port map (diu => rd);
  vjoxoz : entity work.lyrlca
    port map (diu => voumegqrv);
  pyykjpm : entity work.fex
    port map (keysykqa => bojtmvk);
  m : entity work.fex
    port map (keysykqa => akihe);
  
  -- Single-driven assignments
  h <= h;
end yay;



-- Seed after: 5148247119585546171,4080032123900078489
