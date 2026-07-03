-- Seed: 1241403916311001918,2158184632809654795

entity xccdmkwotl is
  port (qr : inout time_vector(0 downto 1); qdpbi : in integer);
end xccdmkwotl;

architecture xfjhpn of xccdmkwotl is
  
begin
  -- Single-driven assignments
  qr <= (others => 0 ns);
end xfjhpn;

entity fovdnvsf is
  port (sirvtpnht : inout real);
end fovdnvsf;

architecture xr of fovdnvsf is
  signal arznwt : integer;
  signal pltfvqier : time_vector(0 downto 1);
  signal jsvrbnsds : integer;
  signal w : time_vector(0 downto 1);
begin
  p : entity work.xccdmkwotl
    port map (qr => w, qdpbi => jsvrbnsds);
  yuqeoxti : entity work.xccdmkwotl
    port map (qr => pltfvqier, qdpbi => arznwt);
  
  -- Single-driven assignments
  arznwt <= jsvrbnsds;
  jsvrbnsds <= jsvrbnsds;
  sirvtpnht <= sirvtpnht;
end xr;



-- Seed after: 13503509407905851505,2158184632809654795
